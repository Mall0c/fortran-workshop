module workshop
	type Fahrzeug
		integer :: Fahrzeugnummer
		integer :: PS
		integer, private :: Geschwindigkeit
		integer, private :: Baujahr
	contains
		procedure :: beschleunigen => beschleunigenFahrzeug
		procedure :: debug
		procedure :: initialize => initFahrzeug
	end type Fahrzeug
	
	type, extends(Fahrzeug) :: Auto
		logical :: Klimaanlage
	contains
		procedure :: beschleunigen => beschleunigenAuto
		procedure :: intialize => initAuto
	end type Auto
	
	contains
	
	subroutine beschleunigenFahrzeug(this, x)
		class(Fahrzeug) :: this
		integer :: x
		print *, "Beschleunigen Fahrzeug"
	end subroutine beschleunigenFahrzeug
	
	subroutine beschleunigenAuto(this, x)
		class(Auto) :: this
		integer :: x
		print *, "Beschleunigen Auto"
	end subroutine beschleunigenAuto
	
	subroutine debug(this)
		class(Fahrzeug) :: this
		print *, this%Fahrzeugnummer, this%PS, this%Geschwindigkeit, this%Baujahr
	end subroutine debug
	
	subroutine initFahrzeug(this, Fahrzeugnummer, PS, Geschwindigkeit, Baujahr, Klimaanlage)
		class(Fahrzeug) :: this
		integer :: Fahrzeugnummer
		integer :: PS
		integer :: Geschwindigkeit
		integer :: Baujahr
		logical :: Klimaanlage
		
		this%Fahrzeugnummer = Fahrzeugnummer
		this%PS = PS
		this%Geschwindigkeit = Geschwindigkeit
		this%Baujahr = Baujahr
	end subroutine initFahrzeug
	
	subroutine initAuto(this, Fahrzeugnummer, PS, Geschwindigkeit, Baujahr, Klimaanlage)
		class(Auto) :: this
		integer :: Fahrzeugnummer
		integer :: PS
		integer :: Geschwindigkeit
		integer :: Baujahr
		logical :: Klimaanlage
		
		call this%Fahrzeug%initialize(Fahrzeugnummer, PS, Geschwindigkeit, Baujahr, Klimaanlage)
		this%Klimaanlage = Klimaanlage
	end subroutine initAuto
end module

program blub
	use workshop
	class(Auto), allocatable :: car
	allocate(car)
	call car%initialize(111, 250, 260, 2011, .true.)
	call car%debug()
end program blub