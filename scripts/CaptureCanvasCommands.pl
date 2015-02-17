open(F,"Graphics/Blank/Canvas.hs");

$state = 0;
while(<F>) {
	if ($state <= 2) {
		$keep .= $_;
	}
	if ($state == 0) {
		if (/HTML5 Canvas assignments:\s+(.*)$/) {
			foreach (split(/,\s*/,$1)) {
				$isAssign{$_} = 1;
			}
			
		}

		next if (! /^data Method/);
		$state = 1;
		next;
	}
	if ($state == 1) {
		if (/^$/ || /Specials/) {
			$state = 2;
			next;
		}
                foreach(keys %dict) {
                        delete($dict{$_});
                }
                if (/forall\s+(\S+)\s+\.\s+(\S+)\s(\S+)\s\=\>/ && $1 eq $3) {
                        $_ = $` . $';
                        $dict{$1} = $2;
                        print "Adding $2 $1 as dictionary\n";
                }

                if (/[\|=]\s+(\S+)\s+(.*\S)\s+\-\-\s+(\S.*)$/ || /[\|=]\s+(\S+)\s+(.*)$/) {
			print "($1)<$2>($3)\n";

			$cmd = $1;
			$args = $2;
			$orig_args = $2;
                        $comment = $3;
			$args =~ s/\((.*)\)/$1/;
			@args = split(/[\(\),]\s*/,$args);
			# name is the lower case version
			$name = $cmd;
			$name =~ s/(\w+)/\l$1/g;
			if ($args eq "") {
				$type = "() -> Canvas ()";
			} else {
				$type = "$orig_args -> Canvas ()";
			}
                        foreach(@args) {
                                if (defined($dict{$_})) {
                                        $type = "$dict{$_} $_ => " . $type;
                                }
                        }

			$header .= "        , $name\n";
			$dsl .= "\n";
                        if ($comment ne "") {
                                $dsl .= "-- | $comment\n";
                        }
			$dsl .= "$name :: $type\n";
			if ($args eq "") {
				$dsl .= "$name () = Method $cmd\n";
				$show .= "  showb $cmd = \"$name()\"\n";
			} else {
				$dsl .= "$name = Method . $cmd\n";
				@ins = ();
				$n = 1;
				foreach (@args) {
					push(@ins,"a$n");
					$n++;
 				}
 				$show .= "  showb ($cmd (" . join(',',@ins) . ")) = \"$name";
				if (defined $isAssign{$cmd}) {
				  $show .= " = ";
				} 
				$show .= "(\" <> ";
				@outs = ();
				$n = 1;
				for $arg (@args) {
                                     if ($arg =~ /^\[(.*)\]$/) {
                                             push(@outs,"jsList js$1 a$n");
                                     } elsif ($arg =~ /^([a-z])(.*)$/) {
                                             push(@outs,"js". uc($1) . $2 . " a$n");
                                     } else {
                                             push(@outs,"js$arg a$n"); 
                                     }
				     $n++;
				}
				$show .= join(" <> singleton ',' <> ",@outs);
				$show .= " <> singleton ')'\n";
			}

		}
	}
	if ($state == 2) {
		if (/AUTO GENERATED AFTER HERE/) {
			$state = 3;
			next;
		}
	}
}


#print "-- DSL\n$dsl\n";
#print "-- Header\n$header\n";
#print "$keep";
#print "\n";
#print "instance Show Method where\n";
#print "$show\n";

open(G,">Graphics/Blank/Generated.hs");
print G "{-# LANGUAGE OverloadedStrings #-}\n";
print G "{-# OPTIONS_GHC -fno-warn-orphans #-}\n";
print G "module Graphics.Blank.Generated where\n";
print G "\n";

print G "import           Data.Monoid ((<>))\n";
print G "import           Data.Text (Text)\n";
print G "\n";
print G "import           Graphics.Blank.Canvas\n";
print G "import           Graphics.Blank.JavaScript\n";
print G "import           Graphics.Blank.Types\n";
print G "import           Graphics.Blank.Types.CSS\n";
print G "import           Graphics.Blank.Types.Font\n";
print G "\n";
print G "import           Prelude hiding (Show)\n";
print G "\n";
print G "import qualified Text.Show as S (Show)\n";
print G "import qualified Text.Show.Text as T (Show)\n";
print G "import           Text.Show.Text (FromTextShow(..), showb, singleton)\n";

print G "\n";
print G "instance S.Show Method where\n";
print G "  showsPrec p = showsPrec p . FromTextShow\n";
print G "\n";
print G "instance T.Show Method where\n";
print G "$show\n";
print G "-- DSL\n$dsl\n";

close(G);
