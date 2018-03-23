lambda-pro:
	swipl -g repl --stand_alone=true -o lambda-pro -O -c load.pl

test:
	swipl -g test -g halt test.pl
