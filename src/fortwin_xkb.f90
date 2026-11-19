module fortwin_xkb
    !! The libxkbcommon calls fortwin needs for keyboard layouts and Compose
    !! sequences, loaded at run time; without the library fortwin reads the
    !! core keyboard mapping.
    use iso_c_binding
    use fortwin_xcb, only: dlopen, dlsym, xcb_cookie_t, xcb_flush
    implicit none
    public

    integer, parameter :: XKB_COMPOSE_NOTHING = 0, XKB_COMPOSE_COMPOSING = 1, &
        XKB_COMPOSE_COMPOSED = 2, XKB_COMPOSE_CANCELLED = 3
    integer, parameter :: XKB_COMPOSE_FEED_IGNORED = 0

    abstract interface
        type(c_ptr) function fn_ptr_int(flags) bind(C)
            import :: c_ptr, c_int
            integer(c_int), value :: flags
        end function
        subroutine fn_unref(p) bind(C)
            import :: c_ptr
            type(c_ptr), value :: p
        end subroutine
        integer(c_int) function fn_setup(conn, major, minor, flags, major_out, minor_out, &
                event_out, error_out) bind(C)
            import :: c_ptr, c_int, c_int16_t
            type(c_ptr), value :: conn
            integer(c_int16_t), value :: major, minor
            integer(c_int), value :: flags
            type(c_ptr), value :: major_out, minor_out, event_out, error_out
        end function
        integer(c_int32_t) function fn_device_id(conn) bind(C)
            import :: c_ptr, c_int32_t
            type(c_ptr), value :: conn
        end function
        type(c_ptr) function fn_keymap_new(ctx, conn, device, flags) bind(C)
            import :: c_ptr, c_int32_t, c_int
            type(c_ptr), value :: ctx, conn
            integer(c_int32_t), value :: device
            integer(c_int), value :: flags
        end function
        type(c_ptr) function fn_state_new(keymap) bind(C)
            import :: c_ptr
            type(c_ptr), value :: keymap
        end function
        integer(c_int) function fn_update_mask(state, depressed, latched, locked, &
                depressed_layout, latched_layout, locked_layout) bind(C)
            import :: c_ptr, c_int, c_int32_t
            type(c_ptr), value :: state
            integer(c_int32_t), value :: depressed, latched, locked, depressed_layout, &
                latched_layout, locked_layout
        end function
        integer(c_int32_t) function fn_state_key_u32(state, key) bind(C)
            import :: c_ptr, c_int32_t
            type(c_ptr), value :: state
            integer(c_int32_t), value :: key
        end function
        integer(c_int) function fn_state_key_utf8(state, key, buf, size) bind(C)
            import :: c_ptr, c_int, c_int32_t, c_size_t, c_char
            type(c_ptr), value :: state
            integer(c_int32_t), value :: key
            character(kind=c_char) :: buf(*)
            integer(c_size_t), value :: size
        end function
        integer(c_int) function fn_syms_by_level(keymap, key, layout, level, syms) bind(C)
            import :: c_ptr, c_int, c_int32_t
            type(c_ptr), value :: keymap
            integer(c_int32_t), value :: key, layout, level
            type(c_ptr), intent(out) :: syms
        end function
        integer(c_int32_t) function fn_sym_u32(sym) bind(C)
            import :: c_int32_t
            integer(c_int32_t), value :: sym
        end function
        type(c_ptr) function fn_table_new(ctx, locale, flags) bind(C)
            import :: c_ptr, c_char, c_int
            type(c_ptr), value :: ctx
            character(kind=c_char), intent(in) :: locale(*)
            integer(c_int), value :: flags
        end function
        type(c_ptr) function fn_compose_new(table, flags) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: table
            integer(c_int), value :: flags
        end function
        integer(c_int) function fn_feed(state, sym) bind(C)
            import :: c_ptr, c_int, c_int32_t
            type(c_ptr), value :: state
            integer(c_int32_t), value :: sym
        end function
        integer(c_int) function fn_status(state) bind(C)
            import :: c_ptr, c_int
            type(c_ptr), value :: state
        end function
        type(xcb_cookie_t) function fn_select_events(conn, device, affect_which, clear, &
                select_all, affect_map, map, details) bind(C)
            import :: c_ptr, c_int16_t, xcb_cookie_t
            type(c_ptr), value :: conn
            integer(c_int16_t), value :: device, affect_which, clear, select_all, affect_map, map
            type(c_ptr), value :: details
        end function
        integer(c_int) function fn_compose_utf8(state, buf, size) bind(C)
            import :: c_ptr, c_int, c_size_t, c_char
            type(c_ptr), value :: state
            character(kind=c_char) :: buf(*)
            integer(c_size_t), value :: size
        end function
    end interface

    procedure(fn_ptr_int), pointer :: xkb_context_new => null()
    procedure(fn_unref), pointer :: xkb_context_unref => null(), xkb_keymap_unref => null(), &
        xkb_state_unref => null(), xkb_compose_table_unref => null(), &
        xkb_compose_state_unref => null(), xkb_compose_state_reset => null()
    procedure(fn_setup), pointer :: xkb_x11_setup_xkb_extension => null()
    procedure(fn_device_id), pointer :: xkb_x11_get_core_keyboard_device_id => null()
    procedure(fn_keymap_new), pointer :: xkb_x11_keymap_new_from_device => null()
    procedure(fn_state_new), pointer :: xkb_state_new => null()
    procedure(fn_update_mask), pointer :: xkb_state_update_mask => null()
    procedure(fn_state_key_u32), pointer :: xkb_state_key_get_one_sym => null(), &
        xkb_state_key_get_layout => null()
    procedure(fn_state_key_utf8), pointer :: xkb_state_key_get_utf8 => null()
    procedure(fn_state_key_u32), pointer :: xkb_keymap_num_layouts_for_key => null()
    procedure(fn_syms_by_level), pointer :: xkb_keymap_key_get_syms_by_level => null()
    procedure(fn_sym_u32), pointer :: xkb_keysym_to_utf32 => null()
    procedure(fn_table_new), pointer :: xkb_compose_table_new_from_locale => null()
    procedure(fn_compose_new), pointer :: xkb_compose_state_new => null()
    procedure(fn_feed), pointer :: xkb_compose_state_feed => null()
    procedure(fn_status), pointer :: xkb_compose_state_get_status => null()
    procedure(fn_compose_utf8), pointer :: xkb_compose_state_get_utf8 => null()
    procedure(fn_select_events), pointer :: xcb_xkb_select_events => null()

    type :: xkb_t
        !! A keyboard: its layouts, their state and the Compose sequences of the locale.
        type(c_ptr) :: ctx = c_null_ptr, keymap = c_null_ptr, state = c_null_ptr, &
            table = c_null_ptr, compose = c_null_ptr
        integer(c_int32_t) :: device = -1
        integer :: event_base = -1 ! XKB events: event_base, then their kind in byte 2
    end type xkb_t

contains

    logical function xkb_load()
        type(c_ptr), save :: lib = c_null_ptr, lib_x11 = c_null_ptr, lib_xcb_xkb = c_null_ptr
        logical, save :: tried = .false.
        xkb_load = c_associated(lib_x11)
        if (tried) return
        tried = .true.
        lib = dlopen("libxkbcommon.so.0" // c_null_char, 2_c_int)
        if (.not. c_associated(lib)) return
        lib_x11 = dlopen("libxkbcommon-x11.so.0" // c_null_char, 2_c_int)
        if (.not. c_associated(lib_x11)) return
        ! An XKB client no longer gets MappingNotify: layout changes come as XKB events.
        lib_xcb_xkb = dlopen("libxcb-xkb.so.1" // c_null_char, 2_c_int)
        if (.not. c_associated(lib_xcb_xkb)) then
            lib_x11 = c_null_ptr
            return
        end if
        call c_f_procpointer(dlsym(lib_xcb_xkb, "xcb_xkb_select_events" // c_null_char), &
            xcb_xkb_select_events)
        call c_f_procpointer(dlsym(lib, "xkb_context_new" // c_null_char), xkb_context_new)
        call c_f_procpointer(dlsym(lib, "xkb_context_unref" // c_null_char), xkb_context_unref)
        call c_f_procpointer(dlsym(lib, "xkb_keymap_unref" // c_null_char), xkb_keymap_unref)
        call c_f_procpointer(dlsym(lib, "xkb_state_unref" // c_null_char), xkb_state_unref)
        call c_f_procpointer(dlsym(lib, "xkb_state_new" // c_null_char), xkb_state_new)
        call c_f_procpointer(dlsym(lib, "xkb_state_update_mask" // c_null_char), xkb_state_update_mask)
        call c_f_procpointer(dlsym(lib, "xkb_state_key_get_one_sym" // c_null_char), &
            xkb_state_key_get_one_sym)
        call c_f_procpointer(dlsym(lib, "xkb_state_key_get_layout" // c_null_char), &
            xkb_state_key_get_layout)
        call c_f_procpointer(dlsym(lib, "xkb_state_key_get_utf8" // c_null_char), xkb_state_key_get_utf8)
        call c_f_procpointer(dlsym(lib, "xkb_keymap_num_layouts_for_key" // c_null_char), &
            xkb_keymap_num_layouts_for_key)
        call c_f_procpointer(dlsym(lib, "xkb_keymap_key_get_syms_by_level" // c_null_char), &
            xkb_keymap_key_get_syms_by_level)
        call c_f_procpointer(dlsym(lib, "xkb_keysym_to_utf32" // c_null_char), xkb_keysym_to_utf32)
        call c_f_procpointer(dlsym(lib, "xkb_compose_table_new_from_locale" // c_null_char), &
            xkb_compose_table_new_from_locale)
        call c_f_procpointer(dlsym(lib, "xkb_compose_table_unref" // c_null_char), xkb_compose_table_unref)
        call c_f_procpointer(dlsym(lib, "xkb_compose_state_new" // c_null_char), xkb_compose_state_new)
        call c_f_procpointer(dlsym(lib, "xkb_compose_state_unref" // c_null_char), xkb_compose_state_unref)
        call c_f_procpointer(dlsym(lib, "xkb_compose_state_reset" // c_null_char), xkb_compose_state_reset)
        call c_f_procpointer(dlsym(lib, "xkb_compose_state_feed" // c_null_char), xkb_compose_state_feed)
        call c_f_procpointer(dlsym(lib, "xkb_compose_state_get_status" // c_null_char), &
            xkb_compose_state_get_status)
        call c_f_procpointer(dlsym(lib, "xkb_compose_state_get_utf8" // c_null_char), &
            xkb_compose_state_get_utf8)
        call c_f_procpointer(dlsym(lib_x11, "xkb_x11_setup_xkb_extension" // c_null_char), &
            xkb_x11_setup_xkb_extension)
        call c_f_procpointer(dlsym(lib_x11, "xkb_x11_get_core_keyboard_device_id" // c_null_char), &
            xkb_x11_get_core_keyboard_device_id)
        call c_f_procpointer(dlsym(lib_x11, "xkb_x11_keymap_new_from_device" // c_null_char), &
            xkb_x11_keymap_new_from_device)
        xkb_load = associated(xkb_x11_keymap_new_from_device) .and. &
            associated(xkb_compose_state_get_utf8) .and. associated(xcb_xkb_select_events)
        if (.not. xkb_load) lib_x11 = c_null_ptr
    end function xkb_load

    logical function xkb_open(k, conn) result(ok)
        !! The keyboard of an X connection, or .false. without libxkbcommon or XKB.
        type(xkb_t), intent(inout) :: k
        type(c_ptr), intent(in) :: conn
        ! NewKeyboardNotify and MapNotify
        integer(c_int16_t), parameter :: EVENTS = 3
        integer(c_int8_t), target :: base
        type(xcb_cookie_t) :: ck
        ok = .false.
        if (.not. xkb_load()) return
        if (xkb_x11_setup_xkb_extension(conn, 1_c_int16_t, 0_c_int16_t, 0_c_int, c_null_ptr, &
            c_null_ptr, c_loc(base), c_null_ptr) /= 1) return
        k%event_base = iand(int(base), 255)
        k%device = xkb_x11_get_core_keyboard_device_id(conn)
        if (k%device < 0) return
        ck = xcb_xkb_select_events(conn, int(k%device, c_int16_t), EVENTS, 0_c_int16_t, EVENTS, &
            0_c_int16_t, 0_c_int16_t, c_null_ptr)
        if (xcb_flush(conn) > 0) continue
        k%ctx = xkb_context_new(0_c_int)
        if (.not. c_associated(k%ctx)) return
        call compose_open(k)
        ok = xkb_reload(k, conn)
    end function xkb_open

    logical function xkb_reload(k, conn) result(ok)
        !! The keymap again, after the layout changed.
        type(xkb_t), intent(inout) :: k
        type(c_ptr), intent(in) :: conn
        type(c_ptr) :: keymap
        ok = .false.
        if (.not. c_associated(k%ctx)) return
        keymap = xkb_x11_keymap_new_from_device(k%ctx, conn, k%device, 0_c_int)
        if (.not. c_associated(keymap)) return
        if (c_associated(k%state)) call xkb_state_unref(k%state)
        if (c_associated(k%keymap)) call xkb_keymap_unref(k%keymap)
        k%keymap = keymap
        k%state = xkb_state_new(keymap)
        ok = c_associated(k%state)
        if (c_associated(k%compose)) call xkb_compose_state_reset(k%compose)
    end function xkb_reload

    subroutine compose_open(k)
        !! The Compose sequences of the locale (LC_ALL, LC_CTYPE, LANG; also
        !! ~/.XCompose), those of en_US.UTF-8 for a locale without its own.
        type(xkb_t), intent(inout) :: k
        character(len=256) :: locale
        integer :: n, j
        character(len=8), parameter :: names(3) = [character(len=8) :: "LC_ALL", "LC_CTYPE", "LANG"]
        locale = ""
        do j = 1, size(names)
            call get_environment_variable(trim(names(j)), locale, n)
            if (n > 0) exit
        end do
        if (len_trim(locale) == 0) locale = "C"
        k%table = xkb_compose_table_new_from_locale(k%ctx, trim(locale) // c_null_char, 0_c_int)
        if (.not. c_associated(k%table)) k%table = xkb_compose_table_new_from_locale(k%ctx, &
            "en_US.UTF-8" // c_null_char, 0_c_int)
        if (c_associated(k%table)) k%compose = xkb_compose_state_new(k%table, 0_c_int)
    end subroutine compose_open

    subroutine xkb_close(k)
        type(xkb_t), intent(inout) :: k
        if (c_associated(k%compose)) call xkb_compose_state_unref(k%compose)
        if (c_associated(k%table)) call xkb_compose_table_unref(k%table)
        if (c_associated(k%state)) call xkb_state_unref(k%state)
        if (c_associated(k%keymap)) call xkb_keymap_unref(k%keymap)
        if (c_associated(k%ctx)) call xkb_context_unref(k%ctx)
        k = xkb_t()
    end subroutine xkb_close

    subroutine xkb_set_state(k, state)
        !! Modifiers and layout from the state of a core event (XKB puts the
        !! layout in bits 13 and 14).
        type(xkb_t), intent(inout) :: k
        integer, intent(in) :: state
        if (xkb_state_update_mask(k%state, int(iand(state, 255), c_int32_t), 0_c_int32_t, &
            0_c_int32_t, 0_c_int32_t, 0_c_int32_t, int(iand(ishft(state, -13), 3), c_int32_t)) &
            < 0) continue
    end subroutine xkb_set_state

    integer function xkb_sym(k, code) result(sym)
        type(xkb_t), intent(in) :: k
        integer, intent(in) :: code
        sym = int(xkb_state_key_get_one_sym(k%state, int(code, c_int32_t)))
    end function xkb_sym

    function xkb_text(k, code) result(s)
        !! The text the key types in the current state.
        type(xkb_t), intent(in) :: k
        integer, intent(in) :: code
        character(len=:), allocatable :: s
        character(kind=c_char) :: buf(64)
        integer :: n, j
        n = xkb_state_key_get_utf8(k%state, int(code, c_int32_t), buf, int(size(buf), c_size_t))
        n = max(0, min(n, size(buf) - 1))
        allocate (character(len=n) :: s)
        do j = 1, n
            s(j:j) = buf(j)
        end do
    end function xkb_text

    integer function xkb_shortcut_char(k, code) result(ucs)
        !! The character a shortcut names on this key: the unshifted one of the
        !! current layout, or, where that is no ASCII character (Cyrillic,
        !! Greek), the first ASCII one of another layout, as GTK does.
        type(xkb_t), intent(in) :: k
        integer, intent(in) :: code
        integer :: layout, n
        layout = int(xkb_state_key_get_layout(k%state, int(code, c_int32_t)))
        ucs = level1(layout)
        if (ucs < 128) return
        n = int(xkb_keymap_num_layouts_for_key(k%keymap, int(code, c_int32_t)))
        do layout = 0, n - 1
            if (level1(layout) > 32 .and. level1(layout) < 127) then
                ucs = level1(layout)
                return
            end if
        end do
    contains
        integer function level1(lay)
            integer, intent(in) :: lay
            type(c_ptr) :: syms
            integer(c_int32_t), pointer :: s(:)
            level1 = 0
            if (xkb_keymap_key_get_syms_by_level(k%keymap, int(code, c_int32_t), &
                int(lay, c_int32_t), 0_c_int32_t, syms) < 1) return
            call c_f_pointer(syms, s, [1])
            level1 = int(xkb_keysym_to_utf32(s(1)))
        end function level1
    end function xkb_shortcut_char

    integer function xkb_feed(k, sym) result(status)
        !! The Compose status after the key; XKB_COMPOSE_NOTHING without a table.
        type(xkb_t), intent(inout) :: k
        integer, intent(in) :: sym
        status = XKB_COMPOSE_NOTHING
        if (.not. c_associated(k%compose)) return
        if (xkb_compose_state_feed(k%compose, int(sym, c_int32_t)) == XKB_COMPOSE_FEED_IGNORED) then
            status = -1
            return
        end if
        status = xkb_compose_state_get_status(k%compose)
    end function xkb_feed

    function xkb_composed(k) result(s)
        type(xkb_t), intent(inout) :: k
        character(len=:), allocatable :: s
        character(kind=c_char) :: buf(64)
        integer :: n, j
        n = xkb_compose_state_get_utf8(k%compose, buf, int(size(buf), c_size_t))
        n = max(0, min(n, size(buf) - 1))
        allocate (character(len=n) :: s)
        do j = 1, n
            s(j:j) = buf(j)
        end do
    end function xkb_composed

    subroutine xkb_compose_reset(k)
        type(xkb_t), intent(inout) :: k
        if (c_associated(k%compose)) call xkb_compose_state_reset(k%compose)
    end subroutine xkb_compose_reset

    function xkb_alone(k, dead) result(s)
        !! What a dead key types before a space (its Compose sequence), e.g. '
        !! for dead_acute.
        type(xkb_t), intent(inout) :: k
        integer, intent(in) :: dead
        character(len=:), allocatable :: s
        s = ""
        call xkb_compose_reset(k)
        if (xkb_feed(k, dead) /= XKB_COMPOSE_COMPOSING) return
        if (xkb_feed(k, 32) == XKB_COMPOSE_COMPOSED) s = xkb_composed(k)
        call xkb_compose_reset(k)
    end function xkb_alone

end module fortwin_xkb
