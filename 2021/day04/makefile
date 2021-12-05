
one: one.hs input
	ghc -o one.out one.hs
	./one.out input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/4/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -f one.out one.hi one.o *.in
