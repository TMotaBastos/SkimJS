function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

array = [6,4,1,10,2,77,8,3];
tan = len(array, 0);

for(var i = 0; i < tan; i = i+1){
	for(var j = 0; j < tan; j = j+1){
		if(array[i]<array[j]){
			aux = array[i];
			array[i] = array[j];
			array[j] = aux;
		}
	}
}

array;