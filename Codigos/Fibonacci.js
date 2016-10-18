var x = 4;
function fibonacci(num){
    if(num == 0){
    	return 0;
    }
    if(num == 1){
        return 1;
    }
    return (fibonacci(y-1))+(fibonacci(y-2));
}
fibonacci(x);