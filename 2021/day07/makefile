
one: one.r input
	Rscript one.r input.in

two: two.m input
	octave two.m

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/7/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -f *.in
