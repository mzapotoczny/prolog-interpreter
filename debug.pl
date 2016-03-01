
[intepreter]. gtrace. run("cos 0 = 1; cos 1 = 123; cos x = cos(x-1);", "cos 3", _).
run("ack 0 n = n+1; ack m 0 = ack (m-1) 1; ack m n = ack (m-1) (ack m (n-1));", "ack 2 2", _).
