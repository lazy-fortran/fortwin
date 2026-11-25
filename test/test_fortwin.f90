#if !defined(_WIN32) && !defined(__APPLE__)
program test_fortwin
    !! On an X server (Xvfb started here unless FORTWIN_TEST_DISPLAY names one):
    !! keys type text with and without Shift and report the unshifted character
    !! for shortcuts; the clipboard and the primary selection pass UTF-8 text,
    !! also 100 kB, between two clients; resizing, waking and closing arrive as
    !! events; dead keys compose accented letters; a window mapped before
    !! anything is drawn shows its background colour; a frame drawn while the
    !! window was hidden is not tiled over a resized or maximized window.
    use iso_c_binding, only: c_int, c_char, c_null_char, c_int8_t, c_int32_t
    use fortwin
    use fortwin_xcb, only: xcb_send_event, xcb_cookie_t, xcb_destroy_window
    use fortwin_keys, only: compose_dead, WIN_KEY_SPACE
    use clip_texts, only: sample
    implicit none
    interface
        integer(c_int) function setenv(name, value, overwrite) bind(C, name="setenv")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: name(*), value(*)
            integer(c_int), value :: overwrite
        end function
    end interface
    type(window_t) :: a, b
    integer :: failures, checked
    character(len=:), allocatable :: display, peer
    character(len=256) :: given
    integer :: n, k
    logical :: own_server

    failures = 0
    checked = 0
    call get_environment_variable("FORTWIN_TEST_DISPLAY", given, n)
    own_server = n == 0
    if (own_server) then
        display = start_xvfb()
    else
        display = trim(given)
    end if
    if (setenv("DISPLAY" // c_null_char, display // c_null_char, 1) /= 0) error stop "setenv"
    call check(open_retrying(a, "fortwin test A"), "window A opens on " // display)
    call check(open_retrying(b, "fortwin test B"), "window B opens")

    ! Keys: plain, with Shift, and the unshifted character of a shifted key.
    call drain(a)
    call win_send_key(a, iachar("a"), 0)
    call expect_text(a, "a", "typed a")
    call win_send_key(a, iachar("A"), 1)
    call expect_text(a, "A", "typed A with Shift")
    call win_send_key(a, iachar("k"), 4)
    call expect_shortcut(a, iachar("k"), WIN_MOD_CTRL, "Ctrl+k reports k and Ctrl, no text")

    ! Selections with another X client in its own process (clip_peer).
    peer = peer_path()
    call check(len(peer) > 0, "clip_peer is built next to this test")
    if (len(peer) > 0) then
        call peer_owns("clipboard", "utf8")
        call check(read_until(a, "clipboard", sample("utf8")), "clipboard UTF-8 from another client")
        call peer_owns("primary", "big")
        call check(read_until(a, "primary", sample("big")), "100 kB primary from another client")
        call win_clipboard_set(a, sample("utf8"))
        call check(peer_reads("clipboard", "utf8"), "another client reads our clipboard")
        call win_primary_set(a, sample("big"))
        call check(peer_reads("primary", "big"), "another client reads our 100 kB primary")
    end if
    call win_clipboard_set(b, "own")
    call check(win_clipboard_get(b) == "own", "own clipboard read back")

    ! Resize, wake, close.
    call drain(a)
    call win_set_size(a, 321, 234)
    call check(wait_for(a, WIN_EV_RESIZE) .and. a%width == 321 .and. a%height == 234, &
        "resize arrives with the new size")
    call win_wake()
    call check(wait_for(a, WIN_EV_WAKE), "win_wake ends win_wait")
    call send_close(a)
    call check(wait_for(a, WIN_EV_CLOSE), "window manager close request")

    ! Dead keys (layouts with them are not on Xvfb's default map).
    call check(compose_dead(int(z'FE57'), iachar("u")) == 252, "dead diaeresis + u = ü")
    call check(compose_dead(int(z'FE51'), iachar("e")) == 233, "dead acute + e = é")
    call check(compose_dead(int(z'FE52'), iachar(" ")) == 94, "dead circumflex + space = ^")
    call check(compose_dead(int(z'FE50'), iachar("x")) == iachar("x"), "no composition: x")
    if (own_server) call check_layouts(display)

    call check_background(display)
    call check_pixels(display)
    call check_frame_not_tiled(display)
    call check_destroyed_window()

    call win_close(a)
    call win_close(b)
    if (own_server) call execute_command_line("kill $(cat " // pidfile() // ") 2>/dev/null; rm -f " &
        // pidfile(), exitstat=k)
    write (*, '(a, i0, a, i0, a)') "fortwin: ", checked, " checks, ", failures, " failures"
    if (failures > 0) error stop 1

contains

    function peer_path() result(p)
        !! clip_peer from the folder of this test's executable.
        character(len=:), allocatable :: p
        character(len=4096) :: exe
        integer :: k
        logical :: there
        call get_command_argument(0, exe)
        k = index(exe, "/", back=.true.)
        p = exe(:k) // "clip_peer"
        inquire (file=p, exist=there)
        if (.not. there) p = ""
    end function peer_path

    subroutine peer_owns(which, kind)
        character(*), intent(in) :: which, kind
        integer :: st
        call execute_command_line(peer // " own " // which // " " // kind // " 3 &", &
            wait=.false., exitstat=st)
    end subroutine peer_owns

    logical function read_until(w, which, want) result(ok)
        !! The selection read until it equals `want` (the peer needs a moment
        !! to start and take ownership).
        type(window_t), intent(inout) :: w
        character(*), intent(in) :: which, want
        character(len=:), allocatable :: got
        real(8) :: t
        ok = .false.
        t = win_time()
        do while (win_time() - t < 3.0d0)
            if (which == "clipboard") then
                got = win_clipboard_get(w)
            else
                got = win_primary_get(w)
            end if
            ok = got == want
            if (ok) return
            call win_wait(w, 0.1)
        end do
    end function read_until

    logical function peer_reads(which, kind) result(ok)
        !! The peer asks for our selection while we answer its requests.
        character(*), intent(in) :: which, kind
        character(len=:), allocatable :: result
        type(win_event_t) :: ev
        character(len=8) :: line
        logical :: there
        integer :: u, st
        real(8) :: t
        result = pidfile() // "." // which
        open (newunit=u, file=result, status="old", iostat=st)
        if (st == 0) close (u, status="delete")
        call execute_command_line(peer // " get " // which // " " // kind // " " // result // &
            " &", wait=.false., exitstat=st)
        ok = .false.
        t = win_time()
        do while (win_time() - t < 4.0d0)
            call win_wait(a, 0.05)
            do while (win_next_event(a, ev))
            end do
            inquire (file=result, exist=there)
            if (there) exit
        end do
        if (.not. there) return
        open (newunit=u, file=result, status="old")
        read (u, '(a)') line
        close (u, status="delete")
        ok = line == "ok"
    end function peer_reads

    subroutine check_destroyed_window()
        type(window_t) :: c
        type(xcb_cookie_t) :: ck
        call check(win_open(c, "fortwin destroyed", 40, 40), "destroy test window opens")
        call drain(c)
        ck = xcb_destroy_window(c%conn, c%id)
        call win_flush(c)
        call check(wait_for(c, WIN_EV_CLOSE), "destroyed window reports close")
        call check(.not. c%mapped, "destroyed window is unmapped")
        call win_close(c)
    end subroutine check_destroyed_window

    subroutine check_pixels(display)
        use iso_fortran_env, only: int8
        character(*), intent(in) :: display
        type(window_t) :: c
        integer(int8) :: pixels(4, 40, 40)
        character(32) :: id
        character(64) :: got
        integer :: st, cs, u
        call execute_command_line("command -v magick >/dev/null 2>&1", exitstat=st, cmdstat=cs)
        if (st /= 0 .or. cs /= 0) return
        call check(win_open(c, "CPU first frame", 40, 40, visible=.false.), "pixel window opens")
        pixels = 0
        pixels(2,:,:) = -1_int8
        pixels(4,:,:) = -1_int8
        call win_present_pixels(c, pixels)
        call win_set_visible(c, .true.)
        call drain(c)
        write(id, '(i0)') c%id
        call execute_command_line("DISPLAY=" // display // " magick x:" // trim(id) // &
            " -format '%[fx:int(255*p{20,20}.r)],%[fx:int(255*p{20,20}.g)]," // &
            "%[fx:int(255*p{20,20}.b)]' info: > fortwin-pixels.txt 2>/dev/null", exitstat=st, cmdstat=cs)
        got = ""
        open(newunit=u, file="fortwin-pixels.txt", action="read", iostat=st)
        if (st == 0) then
            read(u, '(a)', iostat=st) got
            close(u, status="delete")
        end if
        call check(trim(got) == "0,255,0", "hidden window maps with its completed frame: " // trim(got))
        call win_close(c)
    end subroutine

    subroutine check_frame_not_tiled(display)
        !! A green frame drawn while hidden on a red background: a resize after
        !! mapping uncovers red, a window maximized on mapping maps red (the
        !! window manager would size it first), and frames drawn once mapped
        !! still show.
        use iso_fortran_env, only: int8
        character(*), intent(in) :: display
        type(window_t) :: c
        integer(int8) :: pixels(4, 40, 40)
        integer :: st, cs
        call execute_command_line("command -v magick >/dev/null 2>&1", exitstat=st, cmdstat=cs)
        if (st /= 0 .or. cs /= 0) return
        pixels = 0
        pixels(2, :, :) = -1_int8
        pixels(4, :, :) = -1_int8
        call check(win_open(c, "resized frame", 40, 40, visible=.false.), "frame window opens")
        call win_set_background(c, [1.0, 0.0, 0.0])
        call win_present_pixels(c, pixels)
        call win_set_visible(c, .true.)
        call drain(c)
        call win_set_size(c, 80, 80)
        call check(wait_for(c, WIN_EV_RESIZE), "frame window resized")
        call drain(c)
        call check(pixel_at(display, c, 60, 60) == "255,0,0", &
            "a resize uncovers the background, not the frame: " // pixel_at(display, c, 60, 60))
        pixels(2, :, :) = 0
        pixels(3, :, :) = -1_int8
        call win_present_pixels(c, pixels)
        call drain(c)
        call check(pixel_at(display, c, 20, 20) == "0,0,255", &
            "a frame drawn on the mapped window shows: " // pixel_at(display, c, 20, 20))
        call win_close(c)

        call check(win_open(c, "maximized frame", 40, 40, visible=.false.), "maximized window opens")
        call win_set_background(c, [1.0, 0.0, 0.0])
        call win_present_pixels(c, pixels)
        call win_maximize(c)
        call win_set_visible(c, .true.)
        call drain(c)
        call check(pixel_at(display, c, 20, 20) == "255,0,0", &
            "a window maximized on mapping maps in its background: " // pixel_at(display, c, 20, 20))
        call win_close(c)
    end subroutine check_frame_not_tiled

    function pixel_at(display, w, x, y) result(got)
        !! "r,g,b" of the window's pixel, read back with ImageMagick.
        character(*), intent(in) :: display
        type(window_t), intent(in) :: w
        integer, intent(in) :: x, y
        character(len=:), allocatable :: got
        character(len=64) :: line, id, at
        integer :: st, cs, u
        write (id, '(i0)') w%id
        write (at, '(i0,a,i0)') x, ",", y
        call execute_command_line("DISPLAY=" // display // " magick x:" // trim(id) // &
            " -format '%[fx:int(255*p{" // trim(at) // "}.r)],%[fx:int(255*p{" // trim(at) // &
            "}.g)],%[fx:int(255*p{" // trim(at) // "}.b)]' info: > fortwin-pixel-at.txt 2>/dev/null", &
            exitstat=st, cmdstat=cs)
        line = ""
        open (newunit=u, file="fortwin-pixel-at.txt", action="read", iostat=st)
        if (st == 0) then
            read (u, '(a)', iostat=st) line
            close (u, status="delete")
        end if
        got = trim(line)
    end function pixel_at

    subroutine check_background(display)
        !! Mapped without any drawing, the window is its background colour (the X
        !! server paints it; Xvfb has no compositor). Read back with ImageMagick,
        !! which is skipped where it is missing.
        character(*), intent(in) :: display
        type(window_t) :: c
        character(len=64) :: got, id
        integer :: st, cs, u
        call execute_command_line("command -v magick >/dev/null 2>&1", exitstat=st, cmdstat=cs)
        if (st /= 0 .or. cs /= 0) then
            write (*, '(a)') "skip: no ImageMagick to read the background back"
            return
        end if
        call check(win_open(c, "fortwin test C", 40, 40, visible=.false.), "window C opens")
        call win_set_background(c, [1.0, 0.0, 0.0])
        call win_set_visible(c, .true.)
        call drain(c)
        write (id, '(a,z0)') "0x", c%id
        call execute_command_line("DISPLAY=" // display // " magick x:" // trim(id) // &
            " -format '%[fx:int(255*p{20,20}.r)],%[fx:int(255*p{20,20}.g)]," // &
            "%[fx:int(255*p{20,20}.b)]' info: > fortwin-background.txt 2>/dev/null", exitstat=st, cmdstat=cs)
        got = ""
        open (newunit=u, file="fortwin-background.txt", action="read", iostat=st)
        if (st == 0) then
            read (u, '(a)', iostat=st) got
            close (u, status="delete")
        end if
        call check(trim(got) == "255,0,0", "a mapped window shows its background colour: " // &
            trim(got))
        call win_close(c)
    end subroutine check_background

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(*), intent(in) :: what
        checked = checked + 1
        if (ok) return
        failures = failures + 1
        write (*, '(a)') "FAIL: " // what
    end subroutine check

    function pidfile() result(p)
        character(len=:), allocatable :: p
        character(len=256) :: tmp
        call get_environment_variable("TMPDIR", tmp)
        if (len_trim(tmp) == 0) tmp = "/tmp"
        p = trim(tmp) // "/fortwin-test-xvfb.pid"
    end function pidfile

    function start_xvfb() result(d)
        !! Xvfb on the first free display from :91.
        character(len=:), allocatable :: d
        character(len=16) :: num
        integer :: k, st
        logical :: busy
        do k = 91, 140
            write (num, '(i0)') k
            inquire (file="/tmp/.X11-unix/X" // trim(num), exist=busy)
            if (.not. busy) exit
        end do
        d = ":" // trim(num)
        call execute_command_line("Xvfb " // d // " -screen 0 640x480x24 -nolisten tcp " // &
            ">/dev/null 2>&1 & echo $! > " // pidfile(), exitstat=st)
    end function start_xvfb

    logical function open_retrying(w, title) result(ok)
        type(window_t), intent(out) :: w
        character(*), intent(in) :: title
        integer :: k
        real(8) :: t
        do k = 1, 100
            ok = win_open(w, title, 200, 150)
            if (ok) return
            t = win_time()
            do while (win_time() - t < 0.05d0)
            end do
        end do
    end function open_retrying

    subroutine drain(w)
        !! Every queued event, until none arrives for 50 ms.
        type(window_t), intent(inout) :: w
        type(win_event_t) :: ev
        real(8) :: t, quiet
        t = win_time()
        quiet = t
        do while (win_time() - quiet < 0.05d0 .and. win_time() - t < 1.0d0)
            call win_wait(w, 0.02)
            do while (win_next_event(w, ev))
                quiet = win_time()
            end do
        end do
    end subroutine drain

    logical function wait_for(w, kind) result(found)
        type(window_t), intent(inout) :: w
        integer, intent(in) :: kind
        type(win_event_t) :: ev
        real(8) :: t
        found = .false.
        t = win_time()
        do while (win_time() - t < 3.0d0)
            do while (win_next_event(w, ev))
                if (ev%kind == kind) then
                    found = .true.
                    return
                end if
            end do
            call win_wait(w, 0.1)
        end do
    end function wait_for

    subroutine expect_text(w, want, what)
        type(window_t), intent(inout) :: w
        character(*), intent(in) :: want, what
        type(win_event_t) :: ev
        character(len=:), allocatable :: got
        real(8) :: t
        got = ""
        t = win_time()
        do while (win_time() - t < 2.0d0 .and. len(got) < len(want))
            do while (win_next_event(w, ev))
                if (ev%kind == WIN_EV_TEXT) got = got // ev%text
            end do
            if (len(got) < len(want)) call win_wait(w, 0.1)
        end do
        call check(got == want, what // " (got '" // got // "')")
    end subroutine expect_text

    subroutine expect_shortcut(w, codepoint, mods, what)
        !! Key down with the character and modifiers, not marked as a repeat,
        !! then its release, and no text in between.
        type(window_t), intent(inout) :: w
        integer, intent(in) :: codepoint, mods
        character(*), intent(in) :: what
        type(win_event_t) :: ev
        logical :: down, up, text
        real(8) :: t
        down = .false.
        up = .false.
        text = .false.
        t = win_time()
        do while (win_time() - t < 1.0d0 .and. .not. up)
            do while (win_next_event(w, ev))
                if (ev%kind == WIN_EV_KEY_DOWN .and. ev%codepoint == codepoint .and. &
                    iand(ev%mods, mods) == mods .and. .not. ev%repeat) down = .true.
                if (ev%kind == WIN_EV_KEY_UP .and. ev%codepoint == codepoint) up = .true.
                if (ev%kind == WIN_EV_TEXT) text = .true.
            end do
            if (.not. up) call win_wait(w, 0.05)
        end do
        call check(down .and. up .and. .not. text, what)
    end subroutine expect_shortcut

    subroutine check_layouts(display)
        !! Dead keys, AltGr, a second layout and shortcuts on international layouts.
        character(*), intent(in) :: display
        integer, parameter :: ACUTE = int(z'FE51'), CIRCUMFLEX = int(z'FE52'), &
            DIAERESIS = int(z'FE57'), RETURN = int(z'FF0D'), ALTGR = 128, LAYOUT2 = 8192
        integer :: st, cst
        if (.not. layout(display, "us -variant intl")) return
        call win_send_key(a, ACUTE, 0)
        call win_send_key(a, iachar(" "), 0)
        call expect_typed(a, "'", "us intl: dead acute + space = ' without a space key")
        call win_send_key(a, DIAERESIS, 1)
        call win_send_key(a, iachar(" "), 0)
        call expect_typed(a, '"', 'us intl: dead diaeresis + space = " without a space key')
        call win_send_key(a, ACUTE, 0)
        call win_send_key(a, iachar("e"), 0)
        call expect_typed(a, achar(195) // achar(169), "us intl: dead acute + e = e acute")
        call win_send_key(a, ACUTE, 0)
        call win_send_key(a, iachar("t"), 0)
        call expect_typed(a, "'t", "us intl: dead acute + t = 't")
        call win_send_key(a, ACUTE, 0)
        call win_send_key(a, RETURN, 0)
        call win_send_key(a, iachar("e"), 0)
        call expect_typed(a, "e", "us intl: Return cancels a dead key")
        call win_send_key(a, ACUTE, 0)
        if (layout(display, "us -variant intl")) continue
        call win_send_key(a, iachar("e"), 0)
        call expect_typed(a, achar(195) // achar(169), "us intl: a keymap change keeps a dead key")
        if (layout(display, "us")) then
            ! A key remapped by another client, as xdotool does with a spare keycode.
            call execute_command_line("xmodmap -display " // display // &
                " -e 'keycode 8 = dead_acute'", exitstat=st, cmdstat=cst)
            if (cst == 0 .and. st == 0) then
                call drain(a)
                call win_send_key(a, ACUTE, 0)
                call win_send_key(a, iachar("e"), 0)
                call expect_typed(a, achar(195) // achar(169), "a key remapped by xmodmap types")
            end if
        end if
        if (layout(display, "de")) then
            call win_send_key(a, CIRCUMFLEX, 0)
            call win_send_key(a, iachar(" "), 0)
            call expect_typed(a, "^", "de: dead circumflex + space = ^")
            call win_send_key(a, CIRCUMFLEX, 0)
            call win_send_key(a, iachar("a"), 0)
            call expect_typed(a, achar(195) // achar(162), "de: dead circumflex + a = a circumflex")
            call win_send_key(a, 228, 0)
            call expect_typed(a, achar(195) // achar(164), "de: a umlaut key")
            call win_send_key(a, iachar("@"), ALTGR)
            call expect_typed(a, "@", "de: AltGr+q = @")
        end if
        if (layout(display, "pl")) then
            call win_send_key(a, int(z'1B1'), ALTGR)
            call expect_typed(a, achar(196) // achar(133), "pl: AltGr+a = a ogonek")
        end if
        if (layout(display, "us,ru")) then
            call win_send_key(a, iachar("c"), LAYOUT2)
            call expect_typed(a, achar(209) // achar(129), "us,ru: second layout types Cyrillic es")
            call win_send_key(a, iachar("c"), LAYOUT2 + 4)
            call expect_shortcut(a, iachar("c"), WIN_MOD_CTRL, "us,ru: Ctrl on Cyrillic es is Ctrl+c")
        end if
        if (layout(display, "ru")) then
            call win_send_key(a, int(z'6D3'), 4)
            call expect_shortcut(a, iachar("c"), WIN_MOD_CTRL, "ru alone: Ctrl on Cyrillic es is Ctrl+c")
        end if
        if (layout(display, "us -variant dvorak")) then
            call win_send_key(a, iachar("c"), 4)
            call expect_shortcut(a, iachar("c"), WIN_MOD_CTRL, "dvorak: Ctrl+c by its letter")
        end if
        if (layout(display, "us")) continue
    end subroutine check_layouts

    logical function layout(display, args) result(ok)
        !! The keyboard layout of the test server, and the window's keymap reloaded.
        character(*), intent(in) :: display, args
        integer :: st, cst
        st = 1
        call execute_command_line("setxkbmap -display " // display // " " // args, exitstat=st, &
            cmdstat=cst)
        ok = cst == 0 .and. st == 0
        if (.not. ok) write (*, '(a)') "skipped: setxkbmap " // args
        call drain(a)
    end function layout

    subroutine expect_typed(w, want, what)
        !! The text `want`, and no space key pressed while typing it.
        type(window_t), intent(inout) :: w
        character(*), intent(in) :: want, what
        type(win_event_t) :: ev
        character(len=:), allocatable :: got
        logical :: space
        real(8) :: t
        got = ""
        space = .false.
        t = win_time()
        do while (win_time() - t < 0.5d0)
            do while (win_next_event(w, ev))
                if (ev%kind == WIN_EV_TEXT) got = got // ev%text
                if (ev%kind == WIN_EV_KEY_DOWN .and. ev%key == WIN_KEY_SPACE) space = .true.
            end do
            call win_wait(w, 0.05)
        end do
        call check(got == want .and. .not. space, what // " (got '" // got // "')")
    end subroutine expect_typed

    subroutine send_close(w)
        !! The WM_DELETE_WINDOW message a window manager sends.
        type(window_t), intent(inout) :: w
        integer(c_int8_t) :: ev(32)
        type(xcb_cookie_t) :: ck
        ev = 0
        ev(1) = 33
        ev(2) = 32
        ev(5:8) = transfer(w%id, ev(1:4))
        ev(9:12) = transfer(w%a_protocols, ev(1:4))
        ev(13:16) = transfer(w%a_delete, ev(1:4))
        ck = xcb_send_event(w%conn, 0_c_int8_t, w%id, 0_c_int32_t, ev)
        call win_flush(w)
    end subroutine send_close

end program test_fortwin

#else
program test_fortwin
    implicit none
    print *, "X11 host tests apply to Linux. Native hosts use test_native."
end program
#endif
