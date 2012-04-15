open(F,"Graphics/Blank/Canvas.hs");

$state = 0;
while(<F>) {
	if ($state == 0) {

		if (/HTML5 Canvas assignments:\s+(.*)$/) {
			foreach (split(/,\s*/,$1)) {
				$isAssign{$_} = 1;
			}
			
		}


		next if (! /^data Command/);
		$state = 1;
		next;
	}
	if ($state == 1) {
		if (/^$/) {
			$state = 2;
			next;
		}
		if (/[\|=]\s(\S+)\s+(.*)$/) {
			print "($1)<$2>\n";

			$cmd = $1;
			$args = $2;
			$orig_args = $2;
			$args =~ s/\((.*)\)/$1/;
			@args = split(/[\(\),]/,$args);
			# name is the lower case version
			$name = $cmd;
			$name =~ s/(\w+)/\l$1/g;
			if ($args eq "") {
				$type = "Canvas ()";
			} else {
				$type = "$orig_args -> Canvas ()";
			}

			$header .= "        , $name\n";

#			$dsl .= "-- | '$name'\n";
			$dsl .= "\n";
			$dsl .= "$name :: $type\n";
			if ($args eq "") {
				$dsl .= "$name = Command $cmd\n";
				$show .= "  show $cmd = \"c.$name();\"\n";
			} else {
				$dsl .= "$name = Command . $cmd\n";
				@ins = ();
				$n = 1;
				foreach (@args) {
					push(@ins,"a$n");
					$n++;
 				}
 				$show .= "  show ($cmd (" . join(',',@ins) . ")) = \"c.$name";
				if (defined $isAssign{$cmd}) {
				  $show .= " = ";
				} 
				$show .= "(\" ++ ";
				@outs = ();
				$n = 1;
				for $arg (@args) {
				     if ($arg eq "Float") {
				     	push(@outs,"showJ a$n");
				     } elsif ($arg eq "String") {
				     	push(@outs,"show a$n");
				     } else {
				     	push(@outs,"a$n");
				     }
				     $n++;
				}
				$show .= join(" ++ \",\" ++ ",@outs);
				$show .= " ++ \");\"\n";
			}

		}


	}


	
	
}


print "-- DSL\n$dsl\n";

print "-- Show\n$show\n";

print "-- Header\n$header\n";
