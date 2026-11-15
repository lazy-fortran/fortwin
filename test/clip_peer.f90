program clip_peer
    !! The other X client of test_fortwin's selection checks, in its own process:
    !!   clip_peer own clipboard|primary utf8|big SECONDS
    !!   clip_peer get clipboard|primary utf8|big RESULT_FILE
    !! Without arguments (as an ordinary test) it does nothing and passes.
    use fortwin
    use clip_texts, only: sample
    implicit none
    type(window_t) :: w
    type(win_event_t) :: ev
    character(len=256) :: mode, which, kind, last
    character(len=:), allocatable :: got
    real(8) :: t, seconds
    integer :: u, n
    if (command_argument_count() < 4) stop
    call get_command_argument(1, mode)
    call get_command_argument(2, which)
    call get_command_argument(3, kind)
    call get_command_argument(4, last)
    if (.not. win_open(w, "clip peer", 50, 50)) error stop "clip_peer: no display"
    if (mode == "own") then
        if (which == "clipboard") then
            call win_clipboard_set(w, sample(trim(kind)))
        else
            call win_primary_set(w, sample(trim(kind)))
        end if
        read (last, *) seconds
        t = win_time()
        do while (win_time() - t < seconds)
            call win_wait(w, 0.05)
            do while (win_next_event(w, ev))
            end do
        end do
    else
        if (which == "clipboard") then
            got = win_clipboard_get(w)
        else
            got = win_primary_get(w)
        end if
        open (newunit=u, file=trim(last) // ".part", status="replace")
        n = len(got)
        write (u, '(a)') merge("ok ", "bad", got == sample(trim(kind)))
        close (u)
        call rename(trim(last) // ".part", trim(last))
    end if
    call win_close(w)
end program clip_peer
