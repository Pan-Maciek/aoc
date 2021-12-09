
one: One.java input
	javac One.java
	java One input.in

two: two.cs input
	dotnet run input.in

input:
	@cookie=`cat ../cookie`; \
	if [ ! -f input.in ]; then\
		wget 'https://adventofcode.com/2021/day/8/input' -O input.in --header "Cookie: $$cookie";\
	fi

clean:
	rm -Rf *.class bin obj *.in
