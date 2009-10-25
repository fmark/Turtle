#! /bin/bash

# generate disasm and ps files for original files
for f in tests/*.t ; do 
    bb=`basename $f .t`
    b="tests/$bb"
    psize=$(stat -c%s "$b.p")
    if [ $psize -gt 0 ]
    then
	runghc tools/DisASM/DisASM.hs "$b.p" "$b.dis"
	if [ -f "$b.d" ] 
	then
	    tools/PDPlot-Haskell/PDPlot "$b.p" "$b.d" "$b.ps"
	else
	    tools/PDPlot-Haskell/PDPlot "$b.p" /dev/null "$b.ps"
	fi
    fi
done

# now dis and ps with our compiler, and compare with originals
rm -rf tmpt
mkdir tmpt
status=1
for f in tests/*.ps ; do 
    bb=`basename $f .ps`
    bin/turtle "tests/$bb.t" -o "tmpt/$bb.p"
    bin/turtle "tests/$bb.t" -o "tmpt/$bb.dis" -m disasm
    if [ -f "tests/$bb.d" ] 
    then
	tools/PDPlot-Haskell/PDPlot "tmpt/$bb.p" "tests/$bb.d" "tmpt/$bb.ps" 2> /dev/null
    else
	tools/PDPlot-Haskell/PDPlot "tmpt/$bb.p" /dev/null "tmpt/$bb.ps" 2> /dev/null
    fi
    if diff "tmpt/$bb.ps" "tests/$bb.ps" > /dev/null  
    then
	sleep 0
    else
	echo "Test failed: $bb.t" 
	status=2
    fi
done

if [ $status -eq 1 ]
then
    echo "All tests passed."
fi

# cleanup
rm -rf tmpt
