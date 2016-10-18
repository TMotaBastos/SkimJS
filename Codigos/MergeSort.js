function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

function partir(arr, ini, fim){
	var brr = [], cont = 0;
	for(var k = ini; k < fim; k++){
		brr[cont] = arr[k];
		cont++;
	}
	return brr;
}

function mergesort(arr){
	var tam = len(arr,0);
	if(tam < 2) return arr;
	var mid;
	if(tam%2 == 1) mid = (tam-1)/2;
	else mid = tam/2;
	var l = partir(arr, 0, mid);
	var r = partir(arr, mid, tam);
	return merge(mergesort(l), mergesort(r));
}

function merge(l, r){
	var result = [], cont = 0, taml = len(l, 0), tamr = len(r, 0), pl = 0, pr = 0;
	while((taml > 0) && (tamr > 0)){
		if(l[pl] <= r[pr]){
			result[cont] = l[pl];
			taml--;
			pl++;
			cont++;
		}else{
			result[cont] = r[pr];
			tamr--;
			pr++;
			cont++;
		}
	}
	while(taml > 0){
		result[cont] = l[pl];
		taml--;
		pl++;
		cont++;
	}
	while(tamr > 0){
		result[cont] = r[pr];
		tamr--;
		pr++;
		cont++;
	}
	return result;
}
var a = [5,6,1,2,8,7,10,3,9,4];
var top = mergesort(a);
top;
