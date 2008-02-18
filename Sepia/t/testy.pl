sub fib1 {
    my $n = shift;
    if ($n < 2) {
        return $n
    } else {
        return fib1($n-1) + fib1($n-2)
    }
}

sub fib2 {
    my $n = shift;
    die "asdf\n" if $n <= 0;
    if ($n < 2) {
        return $n
    } else {
        return fib2($n-1) + fib2($n-2)
    }
}
