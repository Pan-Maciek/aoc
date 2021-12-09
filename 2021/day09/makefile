
one: one.clj input
	clojure -M one.clj input.in

two: two.rkt input
	racket two.rkt 

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/9/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -Rf *.class bin obj *.in
