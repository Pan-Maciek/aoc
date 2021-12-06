
one: one.vb input
	dotnet run input.in


two: two.f90 input
	gfortran -o two.out two.f90
	./two.out input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/6/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -Rf input.in bin/ obj/ two.out
