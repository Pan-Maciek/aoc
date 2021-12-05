
two: two.asm input
	nasm -felf64 -o two.o two.asm
	ld -o two.out two.o
	./two.out input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/3/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -f *.o *.out *.s *.in
