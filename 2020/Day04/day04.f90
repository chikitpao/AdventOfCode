!   Aoc 2020, Day 4: Passport Processing
!   Author: Chi-Kit Pao
!
!   Commands:
!   gfortran -static day04.f90 -o day04
!   cat input.txt | ./day04
!
!   Output:
!   Question 1: In your batch file, how many passports are valid?
!   Answer:          213
!   Question 2: In your batch file, how many passports are valid?
!   Answer:          147
!
!   Time usage shown via command "time".
!   real	0m0,007s
!   user	0m0,009s
!   sys	0m0,001s
!


program day04
    implicit none
    character(len=128) :: line
    character(len=32) :: s1
    character(len=32) :: s2
    character(len=32) :: s3
    integer :: space_pos
    logical, dimension(8) :: checklist1
    logical, dimension(8) :: checklist2
    integer :: i
    integer :: n
    integer :: stat
    integer :: line_length
    integer :: answer1 = 0
    integer :: answer2 = 0
    integer :: current_index = 0
    character(len=4) :: fields(8) = [ &
        "byr:", &
        "iyr:", &
        "eyr:", &
        "hgt:", &
        "hcl:", &
        "ecl:", &
        "pid:", &
        "cid:" &
    ]

    call clear_list(checklist1)
    call clear_list(checklist2)
    READLINES: do
        read (*, '(A)', iostat=stat) line
        if (stat /= 0) exit READLINES
        line_length = len_trim(line)
        if (line_length == 0) then
            if (check_list(checklist1)) answer1 = answer1 + 1
            call clear_list(checklist1)
            if (check_list(checklist2)) answer2 = answer2 + 1
            call clear_list(checklist2)
        else
            do i = 1, size(fields)
                current_index = index(line, fields(i))
                if(current_index > 0) then
                    checklist1(i) = .TRUE.
                    read(line(current_index + 4 : line_length), "(A)") s1
                    space_pos = index(s1, ' ')
                    if (space_pos > 0) then
                        s2 = s1(1:(space_pos-1))
                    else
                        s2 = s1
                    end if
                    select case (i)
                    case (1)
                        if (verify(trim(s2), "0123456789") == 0) then
                            read(s2, *) n
                            checklist2(i) = (n >= 1920) .AND. (n <= 2002)
                        end if
                    case (2)
                        if (verify(trim(s2), "0123456789") == 0) then
                            read(s2, *) n
                            checklist2(i) = (n >= 2010) .AND. (n <= 2020)
                        end if
                    case (3)
                        if (verify(trim(s2), "0123456789") == 0) then
                            read(s2, *) n
                            checklist2(i) = (n >= 2020) .AND. (n <= 2030)
                        end if
                    case (4)
                        if ((len_trim(s2) >= 3) .AND. (verify(trim(s2(1:(len_trim(s2)-2))), "0123456789") == 0)) then
                            s1 = s2((len_trim(s2)-1):len_trim(s2))
                            if (s1 == "cm") then
                                s3 = s2(1:(len_trim(s2)-2))
                                read(s3, *) n
                                checklist2(i) = (n >= 150) .AND. (n <= 193)
                            else if (s1 == "in") then
                                s3 = s2(1:(len_trim(s2)-2))
                                read(s3, *) n
                                checklist2(i) = (n >= 59) .AND. (n <= 76)
                            end if
                        end if
                    case (5)
                        if ((len_trim(s2) == 7) .AND. (s2(1:1) == "#")) then
                            checklist2(i) = (verify(trim(s2(2:len_trim(s2))), "0123456789abcdef") == 0)
                        end if
                    case (6)
                        select case (trim(s2))
                        case ("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
                            checklist2(i) = .TRUE.
                        case default
                            checklist2(i) = .FALSE.
                        end select
                    case (7)
                        checklist2(i) = (len_trim(s2) == 9) .AND. (verify(trim(s2), "0123456789") == 0)
                    case default    ! i == 8
                        checklist2(i) = .TRUE.
                    end select
                end if
            end do
        end if
    enddo READLINES
    if (check_list(checklist1)) answer1 = answer1 + 1
    if (check_list(checklist2)) answer2 = answer2 + 1

    print*, "Question 1: In your batch file, how many passports are valid?"
    print*, "Answer1: ", answer1
    print*, "Question 2: In your batch file, how many passports are valid?"
    print*, "Answer2: ", answer2
contains

    subroutine clear_list(l)
        implicit none
        logical, dimension(8), intent(inout) :: l
        integer :: i
        do i = 1, size(l)
            l(i) = .FALSE.
        end do
    end subroutine clear_list

    logical function check_list(l)
        implicit none
        logical, dimension(8), intent(in) :: l
        ! integer :: count ! This is not okay. "count" accumulates between calls!
        integer :: count ! This is fine
        integer :: i

        count = 0
        do i = 1, size(l)
            if (l(i)) count = (count + 1)
        end do
        if ((count == 8) .OR. (count == 7 .AND. (.NOT. l(8)))) then
            check_list = .TRUE.
        else
            check_list = .FALSE.
        end if
    end function check_list

end program day04