lambda-pro:
	swipl -g repl --stand_alone=true -o lambda-pro -O -c load.pl

clean:
	rm lambda-pro

test:
	swipl -g test -g halt test.pl
