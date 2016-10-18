function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

function somaArray(a){
    var soma = 0;
    var tan = len(a, 0)
    while(tan > 0){
        soma = soma + a.head;
        a = a.tail;
        tan = len(a, 0)
    }
    return soma;
}

var x = [1,2,3,4,5];
somaArray(x);