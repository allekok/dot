<?php
function nth ($n, $def=false)
{
	global $argv;
	$arg = @$argv[$n];
	return $arg ? $arg : $def;
}

function p ($string='')
{
	echo "$string\n";
}

function nth_pair ($key, $def=false)
{
	global $argv;
	$carry = false;
	foreach ($argv as $o)
	{
		if($carry)
			return $o;
		elseif($key == $o)
			$carry = true;
	}
	return $def;
}

function smart_pair ($key, $def=false)
{
	// Check for all kind of pairs: 
	// ([-x test], [--x test], [--x=test], [-x=test])
	// Is it possible?
}
?>
