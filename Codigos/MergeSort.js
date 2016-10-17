function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

function floorf(num){
	return (num%1);
}

function merge(arr, ini, mid, fim, brr){
	var i = ini, j = (mid+1);
	for(var k = ini; k <= fim; k++){
		if(i < (mid+1) && (j >= fim || arr[i] <= arr[j])){
			brr[k] = arr[i];
			i++;
		}else{
			brr[k] = arr[j];
			j++;
		}
	}
	for(var k = ini; k <= fim; k++){
		arr[k] = brr[k];
	}
	//return brr;
}

function mergesort(arr, ini, fim, brr){
	if(fim - ini == 0) return;
	var mid = (ini+fim)/2;
	mergesort(arr, ini, mid, brr);
	mergesort(arr, (mid+1), fim, brr);
	merge(arr, ini, mid, fim, brr);
}

3 1 2
3   1 2
3   1   2
3   1 2

5 4 6 7

4 5    6 7
      ij
4 5 6 7


4 5   8 6
    i j
4 5 8 6


5 4 8 6
4  5  8  6
   ij
4 5   6  8
        ij
4 5   6 8
  i   j
  
8 4 6 5
8  4  6  5
4 5   8 6
      i j

