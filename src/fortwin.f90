module fortwin
    use fortwin_native, only: native_host_abi
#if defined(_WIN32) || defined(__APPLE__)
    use fortwin_native
    implicit none
    public
#else
    !! Windows on X11 through libxcb, in Fortran: one window with keyboard
    !! (core keymap with Shift, Caps Lock and AltGr levels, dead keys), pointer,
    !! wheel, clipboard and primary selection (owner and requester, incremental
    !! transfers), title, size, maximisation, visibility, display scale, and a
    !! wait that other threads can end through a pipe.
    use iso_fortran_env, only: int8, int16, int32, int64
    use fortwin_xcb
    use fortwin_keys, only: keysym_to_ucs, compose_dead, is_dead_key, WIN_KEY_NONE, &
        named_key
    implicit none
    private
    public :: window_t, win_event_t, win_open, win_close, win_next_event, win_wait, &
        win_wake, win_set_title, win_set_size, win_set_visible, win_set_background, &
        win_maximize, &
        win_clipboard_set, win_clipboard_get, win_primary_set, win_primary_get, win_flush, &
        win_mods_down, win_time, win_send_key
    public :: WIN_EV_NONE, WIN_EV_CLOSE, WIN_EV_RESIZE, WIN_EV_EXPOSE, WIN_EV_MOTION, &
        WIN_EV_BUTTON_DOWN, WIN_EV_BUTTON_UP, WIN_EV_WHEEL, WIN_EV_KEY_DOWN, WIN_EV_KEY_UP, &
        WIN_EV_TEXT, WIN_EV_LEAVE, WIN_EV_FOCUS_IN, WIN_EV_FOCUS_OUT, WIN_EV_WAKE
    public :: WIN_MOD_SHIFT, WIN_MOD_CTRL, WIN_MOD_ALT, WIN_MOD_META

    integer, parameter :: WIN_EV_NONE = 0, WIN_EV_CLOSE = 1, WIN_EV_RESIZE = 2, &
        WIN_EV_EXPOSE = 3, WIN_EV_MOTION = 4, WIN_EV_BUTTON_DOWN = 5, WIN_EV_BUTTON_UP = 6, &
        WIN_EV_WHEEL = 7, WIN_EV_KEY_DOWN = 8, WIN_EV_KEY_UP = 9, WIN_EV_TEXT = 10, &
        WIN_EV_LEAVE = 11, WIN_EV_FOCUS_IN = 12, WIN_EV_FOCUS_OUT = 13, WIN_EV_WAKE = 14
    integer, parameter :: WIN_MOD_SHIFT = 1, WIN_MOD_CTRL = 2, WIN_MOD_ALT = 4, WIN_MOD_META = 8

    type :: win_event_t
        integer :: kind = WIN_EV_NONE
        real :: x = 0, y = 0 ! pointer position in pixels
        integer :: button = 0 ! 1 left, 2 middle, 3 right
        real :: wheel_x = 0, wheel_y = 0 ! wheel steps, y positive away from the user
        integer :: key = WIN_KEY_NONE ! a named key (WIN_KEY_*) or WIN_KEY_NONE
        integer :: codepoint = 0 ! the key's unshifted character, for shortcuts
        integer :: mods = 0 ! WIN_MOD_* held
        logical :: repeat = .false.
        character(len=:), allocatable :: text ! UTF-8 of WIN_EV_TEXT
    end type win_event_t

    type :: selection_t
        character(len=:), allocatable :: text
        logical :: owned = .false.
    end type selection_t

    type :: window_t
        type(c_ptr) :: conn = c_null_ptr
        integer(c_int32_t) :: id = 0, root = 0
        integer :: width = 0, height = 0
        real :: scale = 1.0
        logical :: mapped = .false., visible = .false.
        integer :: mods = 0
        ! atoms
        integer(c_int32_t) :: a_protocols = 0, a_delete = 0, a_clipboard = 0, a_primary = 1, &
            a_targets = 0, a_utf8 = 0, a_text = 0, a_incr = 0, a_property = 0, a_net_name = 0, &
            a_net_state = 0, a_max_h = 0, a_max_v = 0, a_timestamp = 0
        type(selection_t) :: clipboard, primary
        ! keyboard
        integer :: min_keycode = 8, max_keycode = 255, per_keycode = 0
        integer, allocatable :: keysyms(:, :) ! (per_keycode, min_keycode:max_keycode)
        integer :: level3_mask = 0, numlock_mask = 0
        integer :: dead = 0 ! pending dead key keysym
        logical :: down(0:255) = .false.
        integer(c_int32_t) :: last_time = 0
        ! events read ahead and kept for the caller
        type(win_event_t), allocatable :: pending(:)
        integer :: npending = 0
        integer(c_int) :: xfd = -1
    end type window_t

    integer(c_int), save :: wake_fds(2) = -1

contains

    ! --- opening ---------------------------------------------------------------

    logical function win_open(win, title, width, height, visible, class) result(ok)
        !! A window of width x height pixels; hidden until win_set_visible when
        !! `visible` is false. `class` names the program for the window manager.
        type(window_t), intent(out) :: win
        character(*), intent(in) :: title
        integer, intent(in) :: width, height
        logical, intent(in), optional :: visible
        character(*), intent(in), optional :: class
        type(xcb_screen_iterator_t) :: it
        integer(c_int) :: nscreen
        integer(c_int8_t), pointer :: scr(:)
        integer(c_int32_t), target :: values(2), atoms(1), pid(1)
        integer(c_int32_t) :: visual
        type(xcb_cookie_t) :: ck
        character(len=:), allocatable, target :: wm_class
        integer(c_int32_t), parameter :: EVENTS = int(z'00000001', c_int32_t) + & ! KeyPress
            int(z'00000002', c_int32_t) + int(z'00000004', c_int32_t) + & ! KeyRelease, ButtonPress
            int(z'00000008', c_int32_t) + int(z'00000010', c_int32_t) + & ! ButtonRelease, Enter
            int(z'00000020', c_int32_t) + int(z'00000040', c_int32_t) + & ! Leave, PointerMotion
            int(z'00008000', c_int32_t) + int(z'00020000', c_int32_t) + & ! Exposure, StructureNotify
            int(z'00200000', c_int32_t) + int(z'00400000', c_int32_t) ! FocusChange, PropertyChange
        ok = .false.
        if (.not. xcb_load()) return
        win%conn = xcb_connect(c_null_ptr, nscreen)
        if (xcb_connection_has_error(win%conn) /= 0) then
            call xcb_disconnect(win%conn)
            win%conn = c_null_ptr
            return
        end if
        it = xcb_setup_roots_iterator(xcb_get_setup(win%conn))
        call c_f_pointer(it%data, scr, [40])
        win%root = transfer(scr(1:4), win%root)
        visual = transfer(scr(33:36), visual)
        win%id = xcb_generate_id(win%conn)
        ! Background none (no flicker on resize), then the events.
        values = [0_c_int32_t, EVENTS]
        ck = xcb_create_window(win%conn, 0_c_int8_t, win%id, win%root, 0_c_int16_t, &
            0_c_int16_t, int(max(1, width), c_int16_t), int(max(1, height), c_int16_t), &
            0_c_int16_t, 1_c_int16_t, visual, int(z'00000801', c_int32_t), values)
        win%width = max(1, width)
        win%height = max(1, height)
        win%a_protocols = atom(win, "WM_PROTOCOLS")
        win%a_delete = atom(win, "WM_DELETE_WINDOW")
        win%a_clipboard = atom(win, "CLIPBOARD")
        win%a_targets = atom(win, "TARGETS")
        win%a_utf8 = atom(win, "UTF8_STRING")
        win%a_text = atom(win, "TEXT")
        win%a_incr = atom(win, "INCR")
        win%a_property = atom(win, "FORTWIN_SELECTION")
        win%a_net_name = atom(win, "_NET_WM_NAME")
        win%a_net_state = atom(win, "_NET_WM_STATE")
        win%a_max_h = atom(win, "_NET_WM_STATE_MAXIMIZED_HORZ")
        win%a_max_v = atom(win, "_NET_WM_STATE_MAXIMIZED_VERT")
        win%a_timestamp = atom(win, "TIMESTAMP")
        atoms(1) = win%a_delete
        ck = xcb_change_property(win%conn, 0_c_int8_t, win%id, win%a_protocols, 4_c_int32_t, &
            32_c_int8_t, 1_c_int32_t, c_loc(atoms))
        pid(1) = getpid_c()
        ck = xcb_change_property(win%conn, 0_c_int8_t, win%id, atom(win, "_NET_WM_PID"), &
            6_c_int32_t, 32_c_int8_t, 1_c_int32_t, c_loc(pid))
        if (present(class)) then
            ! WM_CLASS: instance and class, each NUL-terminated.
            wm_class = class // c_null_char // class // c_null_char
            ck = xcb_change_property(win%conn, 0_c_int8_t, win%id, 67_c_int32_t, 31_c_int32_t, &
                8_c_int8_t, int(len(wm_class), c_int32_t), c_loc(wm_class))
        end if
        call win_set_title(win, title)
        win%scale = display_scale(win)
        call load_keymap(win)
        allocate (win%pending(16))
        win%xfd = xcb_get_file_descriptor(win%conn)
        if (wake_fds(1) < 0) then
            ! O_NONBLOCK | O_CLOEXEC
            if (c_pipe2(wake_fds, int(o'2004000', c_int)) /= 0) wake_fds = -1
        end if
        if (present(visible)) then
            if (visible) call win_set_visible(win, .true.)
        else
            call win_set_visible(win, .true.)
        end if
        ok = xcb_flush(win%conn) > 0
    end function win_open

    subroutine win_close(win)
        type(window_t), intent(inout) :: win
        type(xcb_cookie_t) :: ck
        if (.not. c_associated(win%conn)) return
        if (win%id /= 0) ck = xcb_destroy_window(win%conn, win%id)
        if (xcb_flush(win%conn) > 0) continue
        call xcb_disconnect(win%conn)
        win%conn = c_null_ptr
        win%id = 0
        win%mapped = .false.
        win%visible = .false.
    end subroutine win_close

    integer(c_int32_t) function getpid_c()
        interface
            integer(c_int) function c_getpid() bind(C, name="getpid")
                import :: c_int
            end function
        end interface
        getpid_c = c_getpid()
    end function getpid_c

    ! --- window properties --------------------------------------------------------

    subroutine win_set_title(win, title)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: title
        character(len=len(title)), target :: t
        type(xcb_cookie_t) :: ck
        t = title
        ck = xcb_change_property(win%conn, 0_c_int8_t, win%id, 39_c_int32_t, 31_c_int32_t, &
            8_c_int8_t, int(len(t), c_int32_t), c_loc(t))
        ck = xcb_change_property(win%conn, 0_c_int8_t, win%id, win%a_net_name, win%a_utf8, &
            8_c_int8_t, int(len(t), c_int32_t), c_loc(t))
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_set_title

    subroutine win_set_size(win, width, height)
        type(window_t), intent(inout) :: win
        integer, intent(in) :: width, height
        integer(c_int32_t) :: values(2)
        type(xcb_cookie_t) :: ck
        values = [int(max(1, width), c_int32_t), int(max(1, height), c_int32_t)]
        ! CONFIG_WINDOW_WIDTH | CONFIG_WINDOW_HEIGHT
        ck = xcb_configure_window(win%conn, win%id, int(4 + 8, c_int16_t), values)
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_set_size

    subroutine win_set_background(win, rgb)
        !! The colour the X server fills the window with where nothing is drawn
        !! yet: when it is mapped and where a resize uncovers it. Without one, a
        !! compositor shows a freshly mapped window white until its first frame.
        !! rgb are 0 to 1 and map to a 24-bit TrueColor pixel.
        type(window_t), intent(inout) :: win
        real, intent(in) :: rgb(3)
        integer(c_int32_t) :: values(1)
        integer :: c(3)
        type(xcb_cookie_t) :: ck
        c = nint(255.0 * max(0.0, min(1.0, rgb)))
        values(1) = int(c(1) * 65536 + c(2) * 256 + c(3), c_int32_t)
        ! CW_BACK_PIXEL
        ck = xcb_change_window_attributes(win%conn, win%id, 2_c_int32_t, values)
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_set_background

    subroutine win_set_visible(win, visible)
        type(window_t), intent(inout) :: win
        logical, intent(in) :: visible
        type(xcb_cookie_t) :: ck
        if (visible .eqv. win%visible) return
        if (visible) then
            ck = xcb_map_window(win%conn, win%id)
        else
            ck = xcb_unmap_window(win%conn, win%id)
        end if
        win%visible = visible
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_set_visible

    subroutine win_maximize(win)
        !! EWMH maximisation: a property before the window is shown, a request to
        !! the window manager afterwards.
        type(window_t), intent(inout) :: win
        integer(c_int32_t), target :: states(2)
        integer(c_int8_t) :: ev(32)
        type(xcb_cookie_t) :: ck
        if (.not. win%visible) then
            states = [win%a_max_v, win%a_max_h]
            ck = xcb_change_property(win%conn, 0_c_int8_t, win%id, win%a_net_state, 4_c_int32_t, &
                32_c_int8_t, 2_c_int32_t, c_loc(states))
        else
            ev = 0
            ev(1) = 33 ! ClientMessage
            ev(2) = 32 ! format
            ev(5:8) = transfer(win%id, ev(1:4))
            ev(9:12) = transfer(win%a_net_state, ev(1:4))
            ev(13:16) = transfer(1_c_int32_t, ev(1:4)) ! _NET_WM_STATE_ADD
            ev(17:20) = transfer(win%a_max_v, ev(1:4))
            ev(21:24) = transfer(win%a_max_h, ev(1:4))
            ev(25:28) = transfer(1_c_int32_t, ev(1:4)) ! source: application
            ! SubstructureRedirect | SubstructureNotify on the root window
            ck = xcb_send_event(win%conn, 0_c_int8_t, win%root, &
                int(z'00180000', c_int32_t), ev)
        end if
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_maximize

    subroutine win_flush(win)
        type(window_t), intent(inout) :: win
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_flush

    real function display_scale(win) result(scale)
        !! Xft.dpi from the RESOURCE_MANAGER property, relative to 96 dpi.
        type(window_t), intent(inout) :: win
        character(len=:), allocatable :: db
        integer :: k, e, dpi, ios
        scale = 1.0
        db = property_text(win, win%root, 23_c_int32_t, 31_c_int32_t)
        k = index(db, "Xft.dpi:")
        if (k == 0) return
        e = index(db(k:), achar(10))
        if (e == 0) e = len(db) - k + 2
        read (db(k + 8:k + e - 2), *, iostat=ios) dpi
        if (ios == 0 .and. dpi > 0) scale = real(dpi) / 96.0
    end function display_scale

    ! --- time and modifiers -------------------------------------------------------

    real(c_double) function win_time()
        !! Seconds on a monotonic clock.
        integer(int64) :: count, rate
        call system_clock(count, rate)
        win_time = real(count, c_double) / real(rate, c_double)
    end function win_time

    integer function win_mods_down(win)
        type(window_t), intent(in) :: win
        win_mods_down = win%mods
    end function win_mods_down

    ! --- events --------------------------------------------------------------------

    logical function win_next_event(win, ev) result(got)
        !! The next event without waiting; false when none is queued.
        type(window_t), intent(inout) :: win
        type(win_event_t), intent(out) :: ev
        type(c_ptr) :: p
        got = .false.
        do
            if (win%npending > 0) then
                ev = win%pending(1)
                win%pending(1:win%npending - 1) = win%pending(2:win%npending)
                win%npending = win%npending - 1
                got = .true.
                return
            end if
            p = xcb_poll_for_event(win%conn)
            if (.not. c_associated(p)) return
            call translate(win, p, .true.)
            call c_free(p)
        end do
    end function win_next_event

    subroutine win_wait(win, timeout)
        !! Block until an X event arrives, another thread calls win_wake, or
        !! `timeout` seconds pass (negative: no limit).
        type(window_t), intent(inout) :: win
        real, intent(in) :: timeout
        type(pollfd_t) :: fds(2)
        integer(c_int) :: ms, n
        integer(int8), target :: drain(64)
        type(c_ptr) :: p
        if (win%npending > 0) return
        ! Events already read by xcb must not wait for the socket.
        p = xcb_poll_for_event(win%conn)
        if (c_associated(p)) then
            call translate(win, p, .true.)
            call c_free(p)
            return
        end if
        if (xcb_flush(win%conn) > 0) continue
        fds(1) = pollfd_t(win%xfd, 1_c_short, 0_c_short) ! POLLIN
        fds(2) = pollfd_t(wake_fds(1), 1_c_short, 0_c_short)
        ms = -1
        if (timeout >= 0.0) ms = int(timeout * 1000.0, c_int)
        n = c_poll(fds, int(merge(2, 1, wake_fds(1) >= 0), c_long), ms)
        if (n > 0 .and. wake_fds(1) >= 0) then
            if (iand(int(fds(2)%revents), 1) /= 0) then
                do while (c_read(wake_fds(1), c_loc(drain), 64_c_size_t) > 0)
                end do
                call push(win, win_event_t(kind=WIN_EV_WAKE))
            end if
        end if
    end subroutine win_wait

    subroutine win_wake() bind(C)
        !! Ends a win_wait from any thread.
        integer(int8), target, save :: one(1) = 1
        if (wake_fds(2) >= 0) then
            if (c_write(wake_fds(2), c_loc(one), 1_c_size_t) < 0) continue
        end if
    end subroutine win_wake

    subroutine push(win, ev)
        type(window_t), intent(inout) :: win
        type(win_event_t), intent(in) :: ev
        if (win%npending == size(win%pending)) win%pending = [win%pending, win%pending]
        win%npending = win%npending + 1
        win%pending(win%npending) = ev
    end subroutine push

    recursive subroutine translate(win, p, peek)
        !! An xcb event into zero or more window events.
        type(window_t), intent(inout) :: win
        type(c_ptr), intent(in) :: p
        logical, intent(in) :: peek
        integer(c_int8_t), pointer :: b(:)
        integer :: kind, w, h, detail, state
        type(win_event_t) :: ev
        type(c_ptr) :: q
        integer(c_int8_t), pointer :: nb(:)
        call c_f_pointer(p, b, [32])
        kind = iand(int(b(1)), 127)
        detail = iand(int(b(2)), 255)
        select case (kind)
        case (2, 3) ! KeyPress, KeyRelease
            state = u16(b, 28)
            win%last_time = transfer(b(5:8), win%last_time)
            call set_mods(win, state)
            if (kind == 3 .and. peek) then
                ! Auto-repeat comes as release and press with one time stamp.
                q = xcb_poll_for_queued_event(win%conn)
                if (c_associated(q)) then
                    call c_f_pointer(q, nb, [32])
                    if (iand(int(nb(1)), 127) == 2 .and. iand(int(nb(2)), 255) == detail .and. &
                        all(nb(5:8) == b(5:8))) then
                        call key_press(win, detail, u16(nb, 28), .true.)
                        call c_free(q)
                        return
                    end if
                    call key_release(win, detail)
                    call translate(win, q, .true.)
                    call c_free(q)
                    return
                end if
            end if
            if (kind == 2) then
                ! A copy: key_press marks the key as down before it reads repeat.
                call key_press(win, detail, state, (win%down(detail)))
            else
                call key_release(win, detail)
            end if
        case (4, 5) ! ButtonPress, ButtonRelease
            call set_mods(win, u16(b, 28))
            ev%x = real(s16(b, 24))
            ev%y = real(s16(b, 26))
            ev%mods = win%mods
            select case (detail)
            case (4, 5, 6, 7)
                if (kind == 4) then
                    ev%kind = WIN_EV_WHEEL
                    if (detail == 4) ev%wheel_y = 1.0
                    if (detail == 5) ev%wheel_y = -1.0
                    if (detail == 6) ev%wheel_x = -1.0
                    if (detail == 7) ev%wheel_x = 1.0
                    call push(win, ev)
                end if
            case default
                ev%kind = merge(WIN_EV_BUTTON_DOWN, WIN_EV_BUTTON_UP, kind == 4)
                ev%button = detail
                call push(win, ev)
            end select
        case (6) ! MotionNotify
            call set_mods(win, u16(b, 28))
            ev%kind = WIN_EV_MOTION
            ev%x = real(s16(b, 24))
            ev%y = real(s16(b, 26))
            call push(win, ev)
        case (8) ! LeaveNotify
            ev%kind = WIN_EV_LEAVE
            call push(win, ev)
        case (9)
            ev%kind = WIN_EV_FOCUS_IN
            call push(win, ev)
        case (10)
            win%down = .false.
            win%mods = 0
            ev%kind = WIN_EV_FOCUS_OUT
            call push(win, ev)
        case (12) ! Expose
            if (u16(b, 16) == 0) then ! last of a series
                ev%kind = WIN_EV_EXPOSE
                call push(win, ev)
            end if
        case (17) ! DestroyNotify
            if (transfer(b(9:12), 0_c_int32_t) == win%id) then
                win%id = 0
                win%mapped = .false.
                win%visible = .false.
                ev%kind = WIN_EV_CLOSE
                call push(win, ev)
            end if
        case (19) ! MapNotify
            win%mapped = .true.
        case (18) ! UnmapNotify
            win%mapped = .false.
        case (22) ! ConfigureNotify
            w = u16(b, 20)
            h = u16(b, 22)
            if (w /= win%width .or. h /= win%height) then
                win%width = w
                win%height = h
                ev%kind = WIN_EV_RESIZE
                call push(win, ev)
            end if
        case (28) ! PropertyNotify
            continue
        case (29) ! SelectionClear
            call selection_cleared(win, transfer(b(13:16), 0_c_int32_t))
        case (30) ! SelectionRequest
            call answer_request(win, b)
        case (33) ! ClientMessage
            if (transfer(b(13:16), 0_c_int32_t) == win%a_delete) then
                ev%kind = WIN_EV_CLOSE
                call push(win, ev)
            end if
        case (34) ! MappingNotify
            call load_keymap(win)
        end select
    end subroutine translate

    subroutine set_mods(win, state)
        type(window_t), intent(inout) :: win
        integer, intent(in) :: state
        win%mods = 0
        if (iand(state, 1) /= 0) win%mods = ior(win%mods, WIN_MOD_SHIFT)
        if (iand(state, 4) /= 0) win%mods = ior(win%mods, WIN_MOD_CTRL)
        if (iand(state, 8) /= 0) win%mods = ior(win%mods, WIN_MOD_ALT)
        if (iand(state, 64) /= 0) win%mods = ior(win%mods, WIN_MOD_META)
    end subroutine set_mods

    ! --- keyboard -------------------------------------------------------------------

    subroutine load_keymap(win)
        !! The core keyboard mapping and which modifier bits are AltGr and NumLock.
        type(window_t), intent(inout) :: win
        integer(c_int8_t), pointer :: setup(:), r(:)
        type(c_ptr) :: reply
        integer :: count, k, j, m, kpm, code, sym
        call c_f_pointer(xcb_get_setup(win%conn), setup, [40])
        win%min_keycode = iand(int(setup(35)), 255)
        win%max_keycode = iand(int(setup(36)), 255)
        count = win%max_keycode - win%min_keycode + 1
        reply = xcb_get_keyboard_mapping_reply(win%conn, xcb_get_keyboard_mapping(win%conn, &
            int(win%min_keycode, c_int8_t), int(count, c_int8_t)), c_null_ptr)
        if (.not. c_associated(reply)) return
        call c_f_pointer(reply, r, [32])
        win%per_keycode = iand(int(r(2)), 255)
        call c_f_pointer(reply, r, [32 + 4 * win%per_keycode * count])
        if (allocated(win%keysyms)) deallocate (win%keysyms)
        allocate (win%keysyms(win%per_keycode, win%min_keycode:win%max_keycode))
        do k = 0, count - 1
            do j = 1, win%per_keycode
                win%keysyms(j, win%min_keycode + k) = transfer(r(33 + 4 * (k * win%per_keycode + &
                    j - 1):36 + 4 * (k * win%per_keycode + j - 1)), 0_c_int32_t)
            end do
        end do
        call c_free(reply)
        ! Modifier bits of ISO_Level3_Shift / Mode_switch and Num_Lock.
        reply = xcb_get_modifier_mapping_reply(win%conn, xcb_get_modifier_mapping(win%conn), &
            c_null_ptr)
        if (.not. c_associated(reply)) return
        call c_f_pointer(reply, r, [32])
        kpm = iand(int(r(2)), 255)
        call c_f_pointer(reply, r, [32 + 8 * kpm])
        win%level3_mask = 0
        win%numlock_mask = 0
        do m = 0, 7
            do k = 0, kpm - 1
                code = iand(int(r(33 + m * kpm + k)), 255)
                if (code < win%min_keycode .or. code > win%max_keycode) cycle
                do j = 1, win%per_keycode
                    sym = win%keysyms(j, code)
                    if (sym == int(z'FE03') .or. sym == int(z'FF7E')) &
                        win%level3_mask = ior(win%level3_mask, ishft(1, m))
                    if (sym == int(z'FF7F')) win%numlock_mask = ior(win%numlock_mask, ishft(1, m))
                end do
            end do
        end do
        call c_free(reply)
    end subroutine load_keymap

    integer function keysym_of(win, code, state) result(sym)
        !! The keysym of a key in the given modifier state (core protocol
        !! rules with XKB's compatibility layout of levels 3 and 4 in columns 5, 6).
        type(window_t), intent(in) :: win
        integer, intent(in) :: code, state
        integer :: col
        logical :: shift, lock, level3
        sym = 0
        if (.not. allocated(win%keysyms)) return
        if (code < win%min_keycode .or. code > win%max_keycode) return
        shift = iand(state, 1) /= 0
        lock = iand(state, 2) /= 0
        level3 = win%level3_mask /= 0 .and. iand(state, win%level3_mask) /= 0
        col = 1
        if (level3 .and. win%per_keycode >= 5) col = 5
        if (shift) col = col + 1
        if (col > win%per_keycode) col = 1
        sym = win%keysyms(col, code)
        if (sym == 0 .and. col > 1) sym = win%keysyms(col - 1, code)
        ! Keypad keys with NumLock give their digits.
        if (iand(state, win%numlock_mask) /= 0 .and. win%numlock_mask /= 0) then
            if (win%keysyms(2, code) >= int(z'FFAA') .and. win%keysyms(2, code) <= int(z'FFB9')) &
                sym = win%keysyms(merge(1, 2, shift), code)
        end if
        ! Caps Lock: lower-case letters become capitals.
        if (lock .and. .not. shift .and. sym >= iachar("a") .and. sym <= iachar("z")) &
            sym = sym - 32
        if (lock .and. .not. shift .and. sym >= 224 .and. sym <= 254 .and. sym /= 247) &
            sym = sym - 32
    end function keysym_of

    subroutine key_press(win, code, state, repeat)
        type(window_t), intent(inout) :: win
        integer, intent(in) :: code, state
        logical, intent(in) :: repeat
        type(win_event_t) :: ev
        integer :: sym, ucs, base
        win%down(code) = .true.
        sym = keysym_of(win, code, state)
        base = keysym_of(win, code, 0)
        ev%kind = WIN_EV_KEY_DOWN
        ev%key = named_key(sym)
        ev%codepoint = keysym_to_ucs(base)
        if (ev%codepoint >= iachar("A") .and. ev%codepoint <= iachar("Z")) &
            ev%codepoint = ev%codepoint + 32
        ev%mods = win%mods
        ev%repeat = repeat
        call push(win, ev)
        ! Text: printable characters without Ctrl, Alt or Super, dead keys composed.
        if (iand(win%mods, WIN_MOD_CTRL + WIN_MOD_ALT + WIN_MOD_META) /= 0) return
        if (is_dead_key(sym)) then
            win%dead = sym
            return
        end if
        ucs = keysym_to_ucs(sym)
        if (ucs < 32 .or. ucs == 127) return
        if (win%dead /= 0) then
            ucs = compose_dead(win%dead, ucs)
            win%dead = 0
        end if
        ev = win_event_t()
        ev%kind = WIN_EV_TEXT
        ev%mods = win%mods
        ev%text = utf8(ucs)
        call push(win, ev)
    end subroutine key_press

    subroutine key_release(win, code)
        type(window_t), intent(inout) :: win
        integer, intent(in) :: code
        type(win_event_t) :: ev
        win%down(code) = .false.
        ev%kind = WIN_EV_KEY_UP
        ev%key = named_key(keysym_of(win, code, 0))
        ev%codepoint = keysym_to_ucs(keysym_of(win, code, 0))
        ev%mods = win%mods
        call push(win, ev)
    end subroutine key_release

    subroutine win_send_key(win, codepoint, state)
        !! A synthetic key press and release of the key that types `codepoint`
        !! with modifier state `state` (X protocol bits), sent to the window
        !! itself (for tests and scripted input).
        type(window_t), intent(inout) :: win
        integer, intent(in) :: codepoint, state
        integer(c_int8_t) :: ev(32)
        integer :: code, k
        type(xcb_cookie_t) :: ck
        code = 0
        do k = win%min_keycode, win%max_keycode
            if (keysym_to_ucs(keysym_of(win, k, state)) == codepoint .or. &
                keysym_of(win, k, state) == codepoint) then
                code = k
                exit
            end if
        end do
        if (code == 0) return
        do k = 2, 3
            ev = 0
            ev(1) = int(k, c_int8_t)
            ev(2) = int(code - 256 * (code / 128), c_int8_t)
            ev(5:8) = transfer(int(k, c_int32_t), ev(1:4))
            ev(9:12) = transfer(win%root, ev(1:4))
            ev(13:16) = transfer(win%id, ev(1:4))
            ev(29:30) = transfer(int(state, c_int16_t), ev(1:2))
            ev(31) = 1
            ck = xcb_send_event(win%conn, 0_c_int8_t, win%id, 0_c_int32_t, ev)
        end do
        if (xcb_flush(win%conn) > 0) continue
    end subroutine win_send_key

    ! --- selections ------------------------------------------------------------------

    subroutine win_clipboard_set(win, text)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: text
        call own(win, win%a_clipboard, text)
    end subroutine win_clipboard_set

    subroutine win_primary_set(win, text)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: text
        call own(win, win%a_primary, text)
    end subroutine win_primary_set

    function win_clipboard_get(win) result(text)
        type(window_t), intent(inout) :: win
        character(len=:), allocatable :: text
        if (win%clipboard%owned) then
            text = win%clipboard%text
        else
            text = request(win, win%a_clipboard)
        end if
    end function win_clipboard_get

    function win_primary_get(win) result(text)
        type(window_t), intent(inout) :: win
        character(len=:), allocatable :: text
        if (win%primary%owned) then
            text = win%primary%text
        else
            text = request(win, win%a_primary)
        end if
    end function win_primary_get

    subroutine own(win, selection, text)
        type(window_t), intent(inout) :: win
        integer(c_int32_t), intent(in) :: selection
        character(*), intent(in) :: text
        type(xcb_cookie_t) :: ck
        if (selection == win%a_clipboard) then
            win%clipboard%text = text
            win%clipboard%owned = .true.
        else
            win%primary%text = text
            win%primary%owned = .true.
        end if
        ck = xcb_set_selection_owner(win%conn, win%id, selection, win%last_time)
        if (xcb_flush(win%conn) > 0) continue
    end subroutine own

    subroutine selection_cleared(win, selection)
        type(window_t), intent(inout) :: win
        integer(c_int32_t), intent(in) :: selection
        if (selection == win%a_clipboard) win%clipboard%owned = .false.
        if (selection == win%a_primary) win%primary%owned = .false.
    end subroutine selection_cleared

    subroutine answer_request(win, b)
        !! Another client asks for our selection: UTF8_STRING, STRING or TEXT
        !! as one property (large texts are rare enough not to use INCR here),
        !! TARGETS as the list of those.
        type(window_t), intent(inout) :: win
        integer(c_int8_t), intent(in) :: b(32)
        integer(c_int32_t) :: requestor, selection, target, property
        integer(c_int32_t), target :: targets(4)
        character(len=:), allocatable, target :: text
        integer(c_int8_t) :: ev(32)
        type(xcb_cookie_t) :: ck
        requestor = transfer(b(13:16), 0_c_int32_t)
        selection = transfer(b(17:20), 0_c_int32_t)
        target = transfer(b(21:24), 0_c_int32_t)
        property = transfer(b(25:28), 0_c_int32_t)
        if (property == 0) property = target
        text = ""
        if (selection == win%a_clipboard .and. win%clipboard%owned) text = win%clipboard%text
        if (selection == win%a_primary .and. win%primary%owned) text = win%primary%text
        if (target == win%a_targets) then
            targets = [win%a_targets, win%a_utf8, 31_c_int32_t, win%a_text]
            ck = xcb_change_property(win%conn, 0_c_int8_t, requestor, property, 4_c_int32_t, &
                32_c_int8_t, 4_c_int32_t, c_loc(targets))
        else if (target == win%a_utf8 .or. target == 31 .or. target == win%a_text) then
            if (len(text) == 0) text = " "
            ck = xcb_change_property(win%conn, 0_c_int8_t, requestor, property, &
                merge(win%a_utf8, 31_c_int32_t, target /= 31), 8_c_int8_t, &
                int(len(text), c_int32_t), c_loc(text))
        else
            property = 0
        end if
        ev = 0
        ev(1) = 31 ! SelectionNotify
        ev(5:8) = b(5:8) ! time
        ev(9:12) = transfer(requestor, ev(1:4))
        ev(13:16) = transfer(selection, ev(1:4))
        ev(17:20) = transfer(target, ev(1:4))
        ev(21:24) = transfer(property, ev(1:4))
        ck = xcb_send_event(win%conn, 0_c_int8_t, requestor, 0_c_int32_t, ev)
        if (xcb_flush(win%conn) > 0) continue
    end subroutine answer_request

    function request(win, selection) result(text)
        !! The owner's text as UTF8_STRING (incremental transfers included),
        !! waiting up to two seconds; other events meanwhile are kept.
        type(window_t), intent(inout) :: win
        integer(c_int32_t), intent(in) :: selection
        character(len=:), allocatable :: text
        type(xcb_cookie_t) :: ck
        type(c_ptr) :: p
        integer(c_int8_t), pointer :: b(:)
        real(c_double) :: deadline
        integer :: kind
        type(pollfd_t) :: fds(1)
        logical :: incr
        text = ""
        ck = xcb_convert_selection(win%conn, win%id, selection, win%a_utf8, win%a_property, &
            win%last_time)
        if (xcb_flush(win%conn) > 0) continue
        deadline = win_time() + 2.0d0
        incr = .false.
        do while (win_time() < deadline)
            p = xcb_poll_for_event(win%conn)
            if (.not. c_associated(p)) then
                fds(1) = pollfd_t(win%xfd, 1_c_short, 0_c_short)
                if (c_poll(fds, 1_c_long, 50_c_int) < 0) exit
                cycle
            end if
            call c_f_pointer(p, b, [32])
            kind = iand(int(b(1)), 127)
            if (kind == 31 .and. .not. incr) then
                if (transfer(b(21:24), 0_c_int32_t) == 0) then
                    call c_free(p)
                    return
                end if
                text = take_property(win, incr)
                call c_free(p)
                if (.not. incr) return
            else if (kind == 28 .and. incr) then
                ! INCR: each new value of the property is the next piece.
                if (transfer(b(5:8), 0_c_int32_t) == win%id .and. &
                    transfer(b(9:12), 0_c_int32_t) == win%a_property .and. b(17) == 0) then
                    block
                        character(len=:), allocatable :: piece
                        logical :: more
                        piece = take_property(win, more)
                        call c_free(p)
                        if (len(piece) == 0) return
                        text = text // piece
                        cycle
                    end block
                end if
                call c_free(p)
            else
                call translate(win, p, .false.)
                call c_free(p)
            end if
        end do
    end function request

    function take_property(win, incr) result(text)
        !! Our selection property's value, deleted after reading; incr tells
        !! that the owner announced an incremental transfer instead.
        type(window_t), intent(inout) :: win
        logical, intent(out) :: incr
        character(len=:), allocatable :: text
        type(c_ptr) :: reply
        integer(c_int8_t), pointer :: r(:)
        integer :: n, typ, k
        type(xcb_cookie_t) :: ck
        incr = .false.
        text = ""
        reply = xcb_get_property_reply(win%conn, xcb_get_property(win%conn, 1_c_int8_t, &
            win%id, win%a_property, 0_c_int32_t, 0_c_int32_t, int(z'3FFFFFFF', c_int32_t)), &
            c_null_ptr)
        if (.not. c_associated(reply)) return
        call c_f_pointer(reply, r, [32])
        typ = transfer(r(9:12), 0_c_int32_t)
        n = transfer(r(17:20), 0_c_int32_t) * max(1, iand(int(r(2)), 255) / 8)
        if (typ == win%a_incr) then
            incr = .true.
            call c_free(reply)
            ck = xcb_delete_property(win%conn, win%id, win%a_property)
            if (xcb_flush(win%conn) > 0) continue
            return
        end if
        call c_f_pointer(reply, r, [32 + n])
        text = repeat(" ", n)
        do k = 1, n
            text(k:k) = achar(iand(int(r(32 + k)), 255))
        end do
        call c_free(reply)
    end function take_property

    function property_text(win, w, prop, typ) result(text)
        type(window_t), intent(inout) :: win
        integer(c_int32_t), intent(in) :: w, prop, typ
        character(len=:), allocatable :: text
        type(c_ptr) :: reply
        integer(c_int8_t), pointer :: r(:)
        integer :: n, k
        text = ""
        reply = xcb_get_property_reply(win%conn, xcb_get_property(win%conn, 0_c_int8_t, w, prop, &
            typ, 0_c_int32_t, 1048576_c_int32_t), c_null_ptr)
        if (.not. c_associated(reply)) return
        call c_f_pointer(reply, r, [32])
        n = transfer(r(17:20), 0_c_int32_t)
        call c_f_pointer(reply, r, [32 + n])
        text = repeat(" ", n)
        do k = 1, n
            text(k:k) = achar(iand(int(r(32 + k)), 255))
        end do
        call c_free(reply)
    end function property_text

    ! --- helpers ------------------------------------------------------------------------

    integer(c_int32_t) function atom(win, name)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: name
        type(c_ptr) :: r
        integer(c_int8_t), pointer :: b(:)
        atom = 0
        r = xcb_intern_atom_reply(win%conn, xcb_intern_atom(win%conn, 0_c_int8_t, &
            int(len(name), c_int16_t), name), c_null_ptr)
        if (.not. c_associated(r)) return
        call c_f_pointer(r, b, [12])
        atom = transfer(b(9:12), atom)
        call c_free(r)
    end function atom

    pure integer function u16(b, off)
        integer(c_int8_t), intent(in) :: b(:)
        integer, intent(in) :: off
        u16 = iand(int(b(off + 1)), 255) + 256 * iand(int(b(off + 2)), 255)
    end function u16

    pure integer function s16(b, off)
        integer(c_int8_t), intent(in) :: b(:)
        integer, intent(in) :: off
        s16 = u16(b, off)
        if (s16 >= 32768) s16 = s16 - 65536
    end function s16

    pure function utf8(ucs) result(s)
        integer, intent(in) :: ucs
        character(len=:), allocatable :: s
        if (ucs < 128) then
            s = achar(ucs)
        else if (ucs < 2048) then
            s = achar(192 + ishft(ucs, -6)) // achar(128 + iand(ucs, 63))
        else if (ucs < 65536) then
            s = achar(224 + ishft(ucs, -12)) // achar(128 + iand(ishft(ucs, -6), 63)) // &
                achar(128 + iand(ucs, 63))
        else
            s = achar(240 + ishft(ucs, -18)) // achar(128 + iand(ishft(ucs, -12), 63)) // &
                achar(128 + iand(ishft(ucs, -6), 63)) // achar(128 + iand(ucs, 63))
        end if
    end function utf8

#endif
end module fortwin
