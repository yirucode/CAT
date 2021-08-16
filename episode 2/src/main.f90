program name
    use vectors
    implicit none
    
    type(vector) :: v
    real :: l

    v = vector(1.0, 2.0, 3.0)
    l = v%length()

    print *, 'Length', l


end program name