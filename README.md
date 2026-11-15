# fortwin

Windows on X11 in Fortran, through libxcb only, which is opened at run time
(a program that never opens a window needs no X libraries): one window with keyboard,
pointer, wheel, clipboard and primary selection, title, size, maximisation,
visibility and display scale, and a wait that other threads can end.

```fortran
use fortwin
type(window_t) :: win
type(win_event_t) :: ev
if (.not. win_open(win, "Hello", 640, 480)) error stop "no X display"
do
    call win_wait(win, -1.0)                   ! or a timeout in seconds
    do while (win_next_event(win, ev))
        if (ev%kind == WIN_EV_CLOSE) stop
        if (ev%kind == WIN_EV_TEXT) print *, ev%text
    end do
end do
```

- Keyboard from the server's core keymap: Shift, Caps Lock and AltGr levels,
  keypad with NumLock, dead keys composed into accented letters, auto-repeat
  recognised; key events carry the unshifted character for shortcuts.
- Clipboard and primary selection as owner and as requester, UTF-8, including
  incremental transfers from other clients.
- `win_wake` (callable from any thread, also from C) ends a `win_wait`.
- The window id and connection serve as the surface of a Vulkan swapchain
  (VK_KHR_xcb_surface), as in [fortgfx](https://github.com/lazy-fortran/fortgfx).

Input methods for Chinese, Japanese and Korean and Wayland are not supported yet.

## Tests

```sh
fo test --all        # or: fpm test; starts its own Xvfb unless FORTWIN_TEST_DISPLAY is set
```

The window test runs on an Xvfb it starts itself and exchanges selections with
a second X client in its own process (`test/clip_peer.f90`).
