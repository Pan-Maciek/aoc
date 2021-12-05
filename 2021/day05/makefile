
one: one.py input
	python3 one.py input.in

two: two.jl input
	julia two.jl input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/5/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -f input.in
