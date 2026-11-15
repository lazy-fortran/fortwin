module fortwin_xcb
    !! The libxcb and libc calls fortwin needs, bound directly (none is variadic).
    use iso_c_binding
    implicit none
    public

    type, bind(C) :: xcb_cookie_t
        integer(c_int) :: sequence
    end type

    type, bind(C) :: xcb_screen_iterator_t
        type(c_ptr) :: data
        integer(c_int) :: rem, index
    end type

    type, bind(C) :: pollfd_t
        integer(c_int) :: fd
        integer(c_short) :: events, revents
    end type

    interface
        type(c_ptr) function xcb_connect(name, screen) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: name
            integer(c_int), intent(out) :: screen
        end function
        integer(c_int) function xcb_connection_has_error(c) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: c
        end function
        subroutine xcb_disconnect(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end subroutine
        type(c_ptr) function xcb_get_setup(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end function
        type(xcb_screen_iterator_t) function xcb_setup_roots_iterator(setup) bind(C)
            import :: c_ptr, xcb_screen_iterator_t
            type(c_ptr), value :: setup
        end function
        integer(c_int32_t) function xcb_generate_id(c) bind(C)
            import :: c_ptr, c_int32_t
            type(c_ptr), value :: c
        end function
        integer(c_int) function xcb_get_file_descriptor(c) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: c
        end function
        integer(c_int) function xcb_flush(c) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: c
        end function
        type(c_ptr) function xcb_poll_for_event(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end function
        type(c_ptr) function xcb_poll_for_queued_event(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end function
        type(xcb_cookie_t) function xcb_create_window(c, depth, wid, parent, x, y, w, h, &
                border, klass, visual, mask, values) bind(C)
            import :: c_ptr, c_int8_t, c_int16_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: depth
            integer(c_int32_t), value :: wid, parent, visual, mask
            integer(c_int16_t), value :: x, y, w, h, border, klass
            integer(c_int32_t), intent(in) :: values(*)
        end function
        type(xcb_cookie_t) function xcb_destroy_window(c, w) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
        end function
        type(xcb_cookie_t) function xcb_map_window(c, w) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
        end function
        type(xcb_cookie_t) function xcb_unmap_window(c, w) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
        end function
        type(xcb_cookie_t) function xcb_configure_window(c, w, mask, values) bind(C)
            import :: c_ptr, c_int16_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
            integer(c_int16_t), value :: mask
            integer(c_int32_t), intent(in) :: values(*)
        end function
        type(xcb_cookie_t) function xcb_change_property(c, mode, w, prop, typ, fmt, n, data) &
                bind(C)
            import :: c_ptr, c_int8_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c, data
            integer(c_int8_t), value :: mode, fmt
            integer(c_int32_t), value :: w, prop, typ, n
        end function
        type(xcb_cookie_t) function xcb_delete_property(c, w, prop) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w, prop
        end function
        type(xcb_cookie_t) function xcb_get_property(c, del, w, prop, typ, offset, length) &
                bind(C)
            import :: c_ptr, c_int8_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: del
            integer(c_int32_t), value :: w, prop, typ, offset, length
        end function
        type(c_ptr) function xcb_get_property_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        type(xcb_cookie_t) function xcb_intern_atom(c, only_if_exists, n, name) bind(C)
            import :: c_ptr, c_int8_t, c_int16_t, c_char, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: only_if_exists
            integer(c_int16_t), value :: n
            character(kind=c_char), intent(in) :: name(*)
        end function
        type(c_ptr) function xcb_intern_atom_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        type(xcb_cookie_t) function xcb_set_selection_owner(c, owner, selection, time) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: owner, selection, time
        end function
        type(xcb_cookie_t) function xcb_convert_selection(c, requestor, selection, target, &
                property, time) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: requestor, selection, target, property, time
        end function
        type(xcb_cookie_t) function xcb_send_event(c, propagate, destination, mask, event) &
                bind(C)
            import :: c_ptr, c_int8_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: propagate
            integer(c_int32_t), value :: destination, mask
            integer(c_int8_t), intent(in) :: event(32)
        end function
        type(xcb_cookie_t) function xcb_get_keyboard_mapping(c, first, count) bind(C)
            import :: c_ptr, c_int8_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: first, count
        end function
        type(c_ptr) function xcb_get_keyboard_mapping_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        type(xcb_cookie_t) function xcb_get_modifier_mapping(c) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c
        end function
        type(c_ptr) function xcb_get_modifier_mapping_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        subroutine c_free(p) bind(C, name="free")
            import :: c_ptr
            type(c_ptr), value :: p
        end subroutine
        integer(c_int) function c_poll(fds, n, timeout) bind(C, name="poll")
            import :: c_int, c_long, pollfd_t
            type(pollfd_t), intent(inout) :: fds(*)
            integer(c_long), value :: n
            integer(c_int), value :: timeout
        end function
        integer(c_int) function c_pipe2(fds, flags) bind(C, name="pipe2")
            import :: c_int
            integer(c_int), intent(out) :: fds(2)
            integer(c_int), value :: flags
        end function
        integer(c_long) function c_write(fd, buf, n) bind(C, name="write")
            import :: c_int, c_long, c_size_t, c_ptr
            integer(c_int), value :: fd
            type(c_ptr), value :: buf
            integer(c_size_t), value :: n
        end function
        integer(c_long) function c_read(fd, buf, n) bind(C, name="read")
            import :: c_int, c_long, c_size_t, c_ptr
            integer(c_int), value :: fd
            type(c_ptr), value :: buf
            integer(c_size_t), value :: n
        end function
    end interface

end module fortwin_xcb
