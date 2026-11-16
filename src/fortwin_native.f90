module fortwin_native
    use iso_c_binding
    use fortwin_keys, only: WIN_KEY_NONE
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

    public :: win_present_pixels
    integer, parameter, public :: native_host_abi = 1
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

    type :: window_t
        type(c_ptr) :: conn = c_null_ptr
        integer(c_int32_t) :: id = 0
        integer :: width = 0, height = 0, mods = 0
        real :: scale = 1.0
        logical :: visible = .false., mapped = .false.
    end type
    type, bind(C) :: native_event_t
        integer(c_int) :: kind, button, key, codepoint, mods, repeated
        real(c_float) :: x, y, wheel_x, wheel_y
        character(c_char) :: text(64)
    end type
    interface
        type(c_ptr) function fw_open(title, w, h) bind(C)
            import c_ptr, c_char, c_int
            character(c_char), intent(in) :: title(*)
            integer(c_int), value :: w, h
        end function
        subroutine fw_close(p) bind(C)
            import c_ptr
            type(c_ptr), value :: p
        end subroutine
        integer(c_int) function fw_event(p, e) bind(C)
            import c_ptr, c_int, native_event_t
            type(c_ptr), value :: p
            type(native_event_t), intent(out) :: e
        end function
        subroutine fw_info(p, w, h, scale) bind(C)
            import c_ptr, c_int, c_float
            type(c_ptr), value :: p
            integer(c_int), intent(out) :: w, h
            real(c_float), intent(out) :: scale
        end subroutine
        subroutine fw_wait(p, ms) bind(C)
            import c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int), value :: ms
        end subroutine
        subroutine fw_wake() bind(C)
        end subroutine
        subroutine fw_title(p, title) bind(C)
            import c_ptr, c_char
            type(c_ptr), value :: p
            character(c_char), intent(in) :: title(*)
        end subroutine
        subroutine fw_size(p, w, h) bind(C)
            import c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int), value :: w, h
        end subroutine
        subroutine fw_visible(p, visible) bind(C)
            import c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int), value :: visible
        end subroutine
        subroutine fw_maximize(p) bind(C)
            import c_ptr
            type(c_ptr), value :: p
        end subroutine
        subroutine fw_present(p, pixels, w, h) bind(C)
            import c_ptr, c_int, c_int8_t
            type(c_ptr), value :: p
            integer(c_int), value :: w, h
            integer(c_int8_t), intent(in) :: pixels(*)
        end subroutine
        subroutine fw_clipboard_set(text) bind(C)
            import c_char
            character(c_char), intent(in) :: text(*)
        end subroutine
        integer(c_int) function fw_clipboard_get(text, cap) bind(C)
            import c_char, c_int
            character(c_char), intent(out) :: text(*)
            integer(c_int), value :: cap
        end function
        integer(c_int) function fw_mods() bind(C)
            import c_int
        end function
        subroutine fw_send_key(p, codepoint, state) bind(C)
            import c_ptr, c_int
            type(c_ptr), value :: p
            integer(c_int), value :: codepoint, state
        end subroutine
        real(c_double) function fw_time() bind(C)
            import c_double
        end function
    end interface
contains
    logical function win_open(win, title, width, height, visible, class) result(ok)
        type(window_t), intent(out) :: win
        character(*), intent(in) :: title
        integer, intent(in) :: width, height
        logical, intent(in), optional :: visible
        character(*), intent(in), optional :: class
        win%conn = fw_open(title // c_null_char, int(width,c_int), int(height,c_int))
        ok = c_associated(win%conn)
        if (.not. ok) return
        call fw_info(win%conn, win%width, win%height, win%scale)
        if (present(visible)) then
            call win_set_visible(win, visible)
        else
            call win_set_visible(win, .true.)
        end if
    end function
    subroutine win_close(win)
        type(window_t), intent(inout) :: win
        if (c_associated(win%conn)) call fw_close(win%conn)
        win%conn = c_null_ptr
    end subroutine
    logical function win_next_event(win, ev) result(ready)
        type(window_t), intent(inout) :: win
        type(win_event_t), intent(out) :: ev
        type(native_event_t) :: e
        integer :: k
        ready = fw_event(win%conn, e) /= 0
        if (.not. ready) return
        ev%kind = e%kind
        ev%x = e%x
        ev%y = e%y
        ev%button = e%button
        ev%wheel_x = e%wheel_x
        ev%wheel_y = e%wheel_y
        ev%key = e%key
        ev%codepoint = e%codepoint
        ev%mods = e%mods
        ev%repeat = e%repeated /= 0
        ev%text = ""
        do k = 1, size(e%text)
            if (e%text(k) == c_null_char) exit
            ev%text = ev%text // e%text(k)
        end do
        call fw_info(win%conn, win%width, win%height, win%scale)
        win%mods = ev%mods
    end function
    subroutine win_wait(win, seconds)
        type(window_t), intent(inout) :: win
        real, intent(in), optional :: seconds
        integer(c_int) :: ms
        ms = -1
        if (present(seconds)) then
            if (seconds >= 0.0) ms = nint(seconds * 1000)
        end if
        call fw_wait(win%conn, ms)
    end subroutine
    subroutine win_wake() bind(C, name="fortwin_native_wake")
        call fw_wake()
    end subroutine
    subroutine win_set_title(win, title)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: title
        call fw_title(win%conn, title // c_null_char)
    end subroutine
    subroutine win_set_size(win, width, height)
        type(window_t), intent(inout) :: win
        integer, intent(in) :: width, height
        call fw_size(win%conn, int(width,c_int), int(height,c_int))
    end subroutine
    subroutine win_set_visible(win, visible)
        type(window_t), intent(inout) :: win
        logical, intent(in) :: visible
        call fw_visible(win%conn, merge(1_c_int,0_c_int,visible))
        win%visible = visible
        win%mapped = visible
    end subroutine
    subroutine win_set_background(win, rgb)
        type(window_t), intent(inout) :: win
        real, intent(in) :: rgb(3)
        ! Native surfaces retain the complete last frame while waiting for paint.
    end subroutine
    subroutine win_maximize(win)
        type(window_t), intent(inout) :: win
        call fw_maximize(win%conn)
    end subroutine
    subroutine win_present_pixels(win, pixels)
        type(window_t), intent(inout) :: win
        integer(c_int8_t), intent(in) :: pixels(:,:,:)
        call fw_present(win%conn, pixels, int(size(pixels,2),c_int), int(size(pixels,3),c_int))
    end subroutine
    subroutine win_flush(win)
        type(window_t), intent(inout) :: win
    end subroutine
    subroutine win_clipboard_set(win, text)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: text
        call fw_clipboard_set(text // c_null_char)
    end subroutine
    function win_clipboard_get(win) result(text)
        type(window_t), intent(inout) :: win
        character(:), allocatable :: text
        character(c_char), allocatable :: buf(:)
        integer :: n, k
        allocate(buf(1))
        n = fw_clipboard_get(buf, 0_c_int)
        if (n <= 0) then
            text = ""
            return
        end if
        deallocate(buf)
        allocate(buf(n+1))
        n = fw_clipboard_get(buf, int(n+1,c_int))
        text = ""
        do k = 1, n
            if (buf(k) == c_null_char) exit
            text = text // buf(k)
        end do
    end function
    subroutine win_primary_set(win, text)
        type(window_t), intent(inout) :: win
        character(*), intent(in) :: text
        call win_clipboard_set(win, text)
    end subroutine
    function win_primary_get(win) result(text)
        type(window_t), intent(inout) :: win
        character(:), allocatable :: text
        text = win_clipboard_get(win)
    end function
    integer function win_mods_down(win)
        type(window_t), intent(in) :: win
        win_mods_down = fw_mods()
    end function
    real(c_double) function win_time()
        win_time = fw_time()
    end function
    subroutine win_send_key(win, codepoint, state)
        type(window_t), intent(inout) :: win
        integer, intent(in) :: codepoint, state
        call fw_send_key(win%conn, int(codepoint,c_int), int(state,c_int))
    end subroutine
end module fortwin_native

