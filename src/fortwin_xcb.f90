module fortwin_xcb
    !! The libxcb calls fortwin needs, loaded at run time, and the libc ones,
    !! bound directly (none is variadic).
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

    abstract interface
        type(c_ptr) function fn_xcb_connect(name, screen) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: name
            integer(c_int), intent(out) :: screen
        end function
        integer(c_int) function fn_xcb_connection_has_error(c) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: c
        end function
        subroutine fn_xcb_disconnect(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end subroutine
        type(c_ptr) function fn_xcb_get_setup(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end function
        type(xcb_screen_iterator_t) function fn_xcb_setup_roots_iterator(setup) bind(C)
            import :: c_ptr, xcb_screen_iterator_t
            type(c_ptr), value :: setup
        end function
        integer(c_int32_t) function fn_xcb_generate_id(c) bind(C)
            import :: c_ptr, c_int32_t
            type(c_ptr), value :: c
        end function
        integer(c_int) function fn_xcb_get_file_descriptor(c) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: c
        end function
        integer(c_int) function fn_xcb_flush(c) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: c
        end function
        type(c_ptr) function fn_xcb_poll_for_event(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end function
        type(c_ptr) function fn_xcb_poll_for_queued_event(c) bind(C)
            import :: c_ptr
            type(c_ptr), value :: c
        end function
        type(xcb_cookie_t) function fn_xcb_create_window(c, depth, wid, parent, x, y, w, h, &
                border, klass, visual, mask, values) bind(C)
            import :: c_ptr, c_int8_t, c_int16_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: depth
            integer(c_int32_t), value :: wid, parent, visual, mask
            integer(c_int16_t), value :: x, y, w, h, border, klass
            integer(c_int32_t), intent(in) :: values(*)
        end function
        type(xcb_cookie_t) function fn_xcb_destroy_window(c, w) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
        end function
        type(xcb_cookie_t) function fn_xcb_map_window(c, w) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
        end function
        type(xcb_cookie_t) function fn_xcb_unmap_window(c, w) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
        end function
        type(xcb_cookie_t) function fn_xcb_configure_window(c, w, mask, values) bind(C)
            import :: c_ptr, c_int16_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w
            integer(c_int16_t), value :: mask
            integer(c_int32_t), intent(in) :: values(*)
        end function
        type(xcb_cookie_t) function fn_xcb_change_property(c, mode, w, prop, typ, fmt, n, data) &
                bind(C)
            import :: c_ptr, c_int8_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c, data
            integer(c_int8_t), value :: mode, fmt
            integer(c_int32_t), value :: w, prop, typ, n
        end function
        type(xcb_cookie_t) function fn_xcb_delete_property(c, w, prop) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: w, prop
        end function
        type(xcb_cookie_t) function fn_xcb_get_property(c, del, w, prop, typ, offset, length) &
                bind(C)
            import :: c_ptr, c_int8_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: del
            integer(c_int32_t), value :: w, prop, typ, offset, length
        end function
        type(c_ptr) function fn_xcb_get_property_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        type(xcb_cookie_t) function fn_xcb_intern_atom(c, only_if_exists, n, name) bind(C)
            import :: c_ptr, c_int8_t, c_int16_t, c_char, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: only_if_exists
            integer(c_int16_t), value :: n
            character(kind=c_char), intent(in) :: name(*)
        end function
        type(c_ptr) function fn_xcb_intern_atom_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        type(xcb_cookie_t) function fn_xcb_set_selection_owner(c, owner, selection, time) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: owner, selection, time
        end function
        type(xcb_cookie_t) function fn_xcb_convert_selection(c, requestor, selection, target, &
                property, time) bind(C)
            import :: c_ptr, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int32_t), value :: requestor, selection, target, property, time
        end function
        type(xcb_cookie_t) function fn_xcb_send_event(c, propagate, destination, mask, event) &
                bind(C)
            import :: c_ptr, c_int8_t, c_int32_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: propagate
            integer(c_int32_t), value :: destination, mask
            integer(c_int8_t), intent(in) :: event(32)
        end function
        type(xcb_cookie_t) function fn_xcb_get_keyboard_mapping(c, first, count) bind(C)
            import :: c_ptr, c_int8_t, xcb_cookie_t
            type(c_ptr), value :: c
            integer(c_int8_t), value :: first, count
        end function
        type(c_ptr) function fn_xcb_get_keyboard_mapping_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
        type(xcb_cookie_t) function fn_xcb_get_modifier_mapping(c) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c
        end function
        type(c_ptr) function fn_xcb_get_modifier_mapping_reply(c, cookie, err) bind(C)
            import :: c_ptr, xcb_cookie_t
            type(c_ptr), value :: c, err
            type(xcb_cookie_t), value :: cookie
        end function
    end interface

    procedure(fn_xcb_connect), pointer :: xcb_connect => null()
    procedure(fn_xcb_connection_has_error), pointer :: xcb_connection_has_error => null()
    procedure(fn_xcb_disconnect), pointer :: xcb_disconnect => null()
    procedure(fn_xcb_get_setup), pointer :: xcb_get_setup => null()
    procedure(fn_xcb_setup_roots_iterator), pointer :: xcb_setup_roots_iterator => null()
    procedure(fn_xcb_generate_id), pointer :: xcb_generate_id => null()
    procedure(fn_xcb_get_file_descriptor), pointer :: xcb_get_file_descriptor => null()
    procedure(fn_xcb_flush), pointer :: xcb_flush => null()
    procedure(fn_xcb_poll_for_event), pointer :: xcb_poll_for_event => null()
    procedure(fn_xcb_poll_for_queued_event), pointer :: xcb_poll_for_queued_event => null()
    procedure(fn_xcb_create_window), pointer :: xcb_create_window => null()
    procedure(fn_xcb_destroy_window), pointer :: xcb_destroy_window => null()
    procedure(fn_xcb_map_window), pointer :: xcb_map_window => null()
    procedure(fn_xcb_unmap_window), pointer :: xcb_unmap_window => null()
    procedure(fn_xcb_configure_window), pointer :: xcb_configure_window => null()
    procedure(fn_xcb_change_property), pointer :: xcb_change_property => null()
    procedure(fn_xcb_delete_property), pointer :: xcb_delete_property => null()
    procedure(fn_xcb_get_property), pointer :: xcb_get_property => null()
    procedure(fn_xcb_get_property_reply), pointer :: xcb_get_property_reply => null()
    procedure(fn_xcb_intern_atom), pointer :: xcb_intern_atom => null()
    procedure(fn_xcb_intern_atom_reply), pointer :: xcb_intern_atom_reply => null()
    procedure(fn_xcb_set_selection_owner), pointer :: xcb_set_selection_owner => null()
    procedure(fn_xcb_convert_selection), pointer :: xcb_convert_selection => null()
    procedure(fn_xcb_send_event), pointer :: xcb_send_event => null()
    procedure(fn_xcb_get_keyboard_mapping), pointer :: xcb_get_keyboard_mapping => null()
    procedure(fn_xcb_get_keyboard_mapping_reply), pointer :: xcb_get_keyboard_mapping_reply => null()
    procedure(fn_xcb_get_modifier_mapping), pointer :: xcb_get_modifier_mapping => null()
    procedure(fn_xcb_get_modifier_mapping_reply), pointer :: xcb_get_modifier_mapping_reply => null()

    interface
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
        type(c_ptr) function dlopen(name, flags) bind(C, name="dlopen")
            import :: c_ptr, c_char, c_int
            character(kind=c_char), intent(in) :: name(*)
            integer(c_int), value :: flags
        end function
        type(c_funptr) function dlsym(handle, name) bind(C, name="dlsym")
            import :: c_ptr, c_funptr, c_char
            type(c_ptr), value :: handle
            character(kind=c_char), intent(in) :: name(*)
        end function
    end interface

contains

    logical function xcb_load()
        !! libxcb, opened at run time: a program that never opens a window
        !! needs no X libraries, and one without them learns it here.
        type(c_ptr), save :: lib = c_null_ptr
        xcb_load = c_associated(lib)
        if (xcb_load) return
        lib = dlopen("libxcb.so.1" // c_null_char, 2_c_int)
        if (.not. c_associated(lib)) lib = dlopen("libxcb.1.dylib" // c_null_char, 2_c_int)
        xcb_load = c_associated(lib)
        if (.not. xcb_load) return
        call c_f_procpointer(dlsym(lib, "xcb_connect" // c_null_char), xcb_connect)
        call c_f_procpointer(dlsym(lib, "xcb_connection_has_error" // c_null_char), xcb_connection_has_error)
        call c_f_procpointer(dlsym(lib, "xcb_disconnect" // c_null_char), xcb_disconnect)
        call c_f_procpointer(dlsym(lib, "xcb_get_setup" // c_null_char), xcb_get_setup)
        call c_f_procpointer(dlsym(lib, "xcb_setup_roots_iterator" // c_null_char), xcb_setup_roots_iterator)
        call c_f_procpointer(dlsym(lib, "xcb_generate_id" // c_null_char), xcb_generate_id)
        call c_f_procpointer(dlsym(lib, "xcb_get_file_descriptor" // c_null_char), xcb_get_file_descriptor)
        call c_f_procpointer(dlsym(lib, "xcb_flush" // c_null_char), xcb_flush)
        call c_f_procpointer(dlsym(lib, "xcb_poll_for_event" // c_null_char), xcb_poll_for_event)
        call c_f_procpointer(dlsym(lib, "xcb_poll_for_queued_event" // c_null_char), xcb_poll_for_queued_event)
        call c_f_procpointer(dlsym(lib, "xcb_create_window" // c_null_char), xcb_create_window)
        call c_f_procpointer(dlsym(lib, "xcb_destroy_window" // c_null_char), xcb_destroy_window)
        call c_f_procpointer(dlsym(lib, "xcb_map_window" // c_null_char), xcb_map_window)
        call c_f_procpointer(dlsym(lib, "xcb_unmap_window" // c_null_char), xcb_unmap_window)
        call c_f_procpointer(dlsym(lib, "xcb_configure_window" // c_null_char), xcb_configure_window)
        call c_f_procpointer(dlsym(lib, "xcb_change_property" // c_null_char), xcb_change_property)
        call c_f_procpointer(dlsym(lib, "xcb_delete_property" // c_null_char), xcb_delete_property)
        call c_f_procpointer(dlsym(lib, "xcb_get_property" // c_null_char), xcb_get_property)
        call c_f_procpointer(dlsym(lib, "xcb_get_property_reply" // c_null_char), xcb_get_property_reply)
        call c_f_procpointer(dlsym(lib, "xcb_intern_atom" // c_null_char), xcb_intern_atom)
        call c_f_procpointer(dlsym(lib, "xcb_intern_atom_reply" // c_null_char), xcb_intern_atom_reply)
        call c_f_procpointer(dlsym(lib, "xcb_set_selection_owner" // c_null_char), xcb_set_selection_owner)
        call c_f_procpointer(dlsym(lib, "xcb_convert_selection" // c_null_char), xcb_convert_selection)
        call c_f_procpointer(dlsym(lib, "xcb_send_event" // c_null_char), xcb_send_event)
        call c_f_procpointer(dlsym(lib, "xcb_get_keyboard_mapping" // c_null_char), xcb_get_keyboard_mapping)
        call c_f_procpointer(dlsym(lib, "xcb_get_keyboard_mapping_reply" // c_null_char), xcb_get_keyboard_mapping_reply)
        call c_f_procpointer(dlsym(lib, "xcb_get_modifier_mapping" // c_null_char), xcb_get_modifier_mapping)
        call c_f_procpointer(dlsym(lib, "xcb_get_modifier_mapping_reply" // c_null_char), xcb_get_modifier_mapping_reply)
    end function xcb_load

end module fortwin_xcb
