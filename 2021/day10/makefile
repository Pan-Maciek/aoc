
one: one.erl input
	escript one.erl input.in

two: lib/two.ex input
	mix escript.build
	./two.out input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/10/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -Rf *.in *.beam _build/ *.out
