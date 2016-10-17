function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}
var a = [1,2,3,4,5,6];
len(a, 0);
