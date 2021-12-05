
one: one.c input
	gcc one.c -O2 -o one.out
	./one.out input.in

two: two.cpp input
	g++ two.cpp -O2 -o two.out
	./two.out input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/1/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -f *.out *.in
