lambda-pro: load.pl
	$(build) > build.out 2>&1 && $(log) && rm build.out

build = swipl -g repl --stand_alone=true -o $@ -O -c $^
logfile = build.out
log = head -n 3 $(logfile) && tail -n 1 $(logfile)

clean:
	touch lambda-pro && rm lambda-pro

test:
	swipl -g test -g halt test.pl
