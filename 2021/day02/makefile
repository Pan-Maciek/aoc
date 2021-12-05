
one: one.coffee input
	coffee one.coffee input.in

two: two.ts input
	deno run --allow-read two.ts input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then \
		wget 'https://adventofcode.com/2021/day/2/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm *.in
