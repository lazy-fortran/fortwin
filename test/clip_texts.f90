module clip_texts
    !! The texts both selection test programs agree on.
    implicit none
    private
    public :: sample
contains
    function sample(kind) result(text)
        character(*), intent(in) :: kind
        character(len=:), allocatable :: text
        if (kind == "big") then
            text = repeat("0123456789abcdef", 6400)
        else
            text = "Grüße, π ≈ 3.14159 — fortwin"
        end if
    end function sample
end module clip_texts
