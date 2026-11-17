program test_native
    use iso_c_binding, only: c_int8_t, c_associated
    use fortwin
#if defined(_WIN32) || defined(__APPLE__)
    implicit none
    type(window_t) :: w
    type(win_event_t) :: e
    integer(c_int8_t), allocatable :: pixels(:,:,:)
    character(:), allocatable :: previous
    integer :: n
    logical :: ready
    if (.not. win_open(w, "Fortwin native test", 320, 240, visible=.false.)) &
        error stop "native window opens"
    if (w%width <= 0 .or. w%height <= 0 .or. w%scale <= 0) error stop "native size"
    if (.not. c_associated(win_native_handle(w))) error stop "native parent handle"
    allocate(pixels(4,w%width,w%height))
    pixels = 0
    pixels(1,:,:) = 127
    pixels(4,:,:) = -1
    call win_present_pixels(w, pixels)
    previous = win_clipboard_get(w)
    call win_clipboard_set(w, "Photo äöü - 測試")
    if (win_clipboard_get(w) /= "Photo äöü - 測試") error stop "Unicode clipboard"
    call win_clipboard_set(w, previous)
    call win_set_title(w, "Fortwin native resized")
    call win_set_size(w, 400, 300)
    do n = 1, 100
        ready = win_next_event(w, e)
        if (w%width == 400 .and. w%height == 300) exit
        call win_wait(w, 0.01)
    end do
    if (w%width /= 400 .or. w%height /= 300) error stop "native resize"
    call win_send_key(w, iachar("o"), 4)
    ready = .false.
    do n = 1, 100
        if (.not. win_next_event(w, e)) cycle
        if (e%kind /= WIN_EV_KEY_DOWN) cycle
        ready = e%codepoint == iachar("o") .and. e%mods == WIN_MOD_CTRL
        exit
    end do
    if (.not. ready) error stop "native key delivery"
    call win_wake()
    call win_wait(w, 0.01)
    call win_close(w)
    if (c_associated(win_native_handle(w))) error stop "closed parent handle"
    print '(a)', "Native window, bitmap, Unicode clipboard resize and key delivery passed"
#else
    implicit none
    print '(a)', "Native host test applies to Windows and macOS"
#endif
end program
