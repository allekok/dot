<?php
function p($string="") {
	echo "$string\n";
}
function nth($n, $def=false) {
	global $argv;
	return isset($argv[$n]) ? $argv[$n] : $def;
}
function nth_pair($key, $def=false) {
	global $argv;
	$carry = false;
	foreach($argv as $o) {
		if($carry)
			return $o;
		if($key == $o)
			$carry = true;
	}
	return $def;
}
?>
