function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

function deletar (a, x) {
	
	var ret = [];
	var i = 0;
	var tan = len(a, 0);

	if(tan > 0){
		do{
			if (a[i] != x) {
				ret = ret.concat(a[i]);
			}
			i++;
		}while(i < tan);
	}
	
	return ret;
}

var array = [4,3,5,4,23,54,7,8,6,3,5,1];
var s = deletar(array, 5);
s;