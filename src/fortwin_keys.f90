module fortwin_keys
    !! X keysyms: named keys, Unicode characters and dead-key composition.
    implicit none
    private
    public :: keysym_to_ucs, named_key, is_dead_key, compose_dead

    integer, parameter, public :: WIN_KEY_NONE = 0, WIN_KEY_ENTER = 1, WIN_KEY_ESCAPE = 2, &
        WIN_KEY_BACKSPACE = 3, WIN_KEY_TAB = 4, WIN_KEY_SPACE = 5, WIN_KEY_DELETE = 6, &
        WIN_KEY_RIGHT = 7, WIN_KEY_LEFT = 8, WIN_KEY_DOWN = 9, WIN_KEY_UP = 10, &
        WIN_KEY_HOME = 11, WIN_KEY_END = 12, WIN_KEY_PAGE_UP = 13, WIN_KEY_PAGE_DOWN = 14, &
        WIN_KEY_INSERT = 15, WIN_KEY_F1 = 21 ! F1..F24 are 21..44

    ! Dead keys: grave, acute, circumflex, tilde, diaeresis, ring, cedilla.
    integer, parameter :: DEAD(7) = [int(z'FE50'), int(z'FE51'), int(z'FE52'), int(z'FE53'), &
        int(z'FE57'), int(z'FE58'), int(z'FE5B')]
    ! The spacing character each dead key types alone.
    integer, parameter :: DEAD_ALONE(7) = [96, 180, 94, 126, 168, 176, 184]
    ! Bases and their composed characters, per dead key (Latin-1 and Latin Extended-A).
    character(*), parameter :: BASES(7) = [character(24) :: "AEIOUaeiou", "AEIOUYaeiouyCcNnSsZz", &
        "AEIOUaeiou", "ANOanoIiUu", "AEIOUaeiouy", "AaUu", "CcSs"]
    integer, parameter :: COMPOSED(20, 7) = reshape([ &
        192, 200, 204, 210, 217, 224, 232, 236, 242, 249, spread(0, 1, 10), &
        193, 201, 205, 211, 218, 221, 225, 233, 237, 243, 250, 253, 262, 263, 323, 324, 346, &
        347, 377, 378, &
        194, 202, 206, 212, 219, 226, 234, 238, 244, 251, spread(0, 1, 10), &
        195, 209, 213, 227, 241, 245, 296, 297, 360, 361, spread(0, 1, 10), &
        196, 203, 207, 214, 220, 228, 235, 239, 246, 252, 255, spread(0, 1, 9), &
        197, 229, 366, 367, spread(0, 1, 16), &
        199, 231, 350, 351, spread(0, 1, 16)], [20, 7])

contains

    pure integer function keysym_to_ucs(sym) result(ucs)
        !! The Unicode character of a keysym, 0 for keys that type none.
        integer, intent(in) :: sym
        ucs = 0
        if (sym >= 32 .and. sym <= 126) then
            ucs = sym
        else if (sym >= 160 .and. sym <= 255) then
            ucs = sym ! Latin-1 keysyms are their code
        else if (iand(sym, int(z'FF000000')) == int(z'01000000')) then
            ucs = iand(sym, int(z'00FFFFFF')) ! direct Unicode keysyms
        else if (sym == int(z'20AC')) then
            ucs = int(z'20AC') ! EuroSign
        else if (sym >= int(z'FFAA') .and. sym <= int(z'FFB9')) then
            ucs = sym - int(z'FF80') ! keypad * + , - . / 0..9
        else if (sym == int(z'FF80')) then
            ucs = 32 ! KP_Space
        end if
    end function keysym_to_ucs

    pure integer function named_key(sym) result(key)
        integer, intent(in) :: sym
        select case (sym)
        case (int(z'FF0D'), int(z'FF8D'))
            key = WIN_KEY_ENTER
        case (int(z'FF1B'))
            key = WIN_KEY_ESCAPE
        case (int(z'FF08'))
            key = WIN_KEY_BACKSPACE
        case (int(z'FF09'), int(z'FE20'))
            key = WIN_KEY_TAB
        case (32)
            key = WIN_KEY_SPACE
        case (int(z'FFFF'), int(z'FF9F'))
            key = WIN_KEY_DELETE
        case (int(z'FF53'), int(z'FF98'))
            key = WIN_KEY_RIGHT
        case (int(z'FF51'), int(z'FF96'))
            key = WIN_KEY_LEFT
        case (int(z'FF54'), int(z'FF99'))
            key = WIN_KEY_DOWN
        case (int(z'FF52'), int(z'FF97'))
            key = WIN_KEY_UP
        case (int(z'FF50'), int(z'FF95'))
            key = WIN_KEY_HOME
        case (int(z'FF57'), int(z'FF9C'))
            key = WIN_KEY_END
        case (int(z'FF55'), int(z'FF9A'))
            key = WIN_KEY_PAGE_UP
        case (int(z'FF56'), int(z'FF9B'))
            key = WIN_KEY_PAGE_DOWN
        case (int(z'FF63'), int(z'FF9E'))
            key = WIN_KEY_INSERT
        case (int(z'FFBE'):int(z'FFD5'))
            key = WIN_KEY_F1 + sym - int(z'FFBE')
        case default
            key = WIN_KEY_NONE
        end select
    end function named_key

    pure logical function is_dead_key(sym)
        integer, intent(in) :: sym
        is_dead_key = any(DEAD == sym)
    end function is_dead_key

    pure integer function compose_dead(dead_sym, ucs) result(out)
        !! A dead key followed by a character: the composed character, the
        !! accent alone before a space, else the character itself.
        integer, intent(in) :: dead_sym, ucs
        integer :: d, j
        out = ucs
        do d = 1, size(DEAD)
            if (DEAD(d) /= dead_sym) cycle
            if (ucs == 32) then
                out = DEAD_ALONE(d)
                return
            end if
            do j = 1, len_trim(BASES(d))
                if (iachar(BASES(d)(j:j)) == ucs .and. COMPOSED(j, d) /= 0) then
                    out = COMPOSED(j, d)
                    return
                end if
            end do
        end do
    end function compose_dead

end module fortwin_keys
