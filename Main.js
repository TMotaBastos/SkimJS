/*
function fat(x){
	if(x == 1) return 1;
	return x*fat(x-1);
}
function bla(x){
	if(x == 1) return 1;
	var oi = x;
	return bla(x-1)*(oi+x);
}
*/

// ----- MERGESORT -----
/*function len(arr, pos){
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
top;*/

// ----- BUBBLESORT -----
/*function len(arr, pos){
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

array;*/

// ----- DELETAR VARIAVEL -----
/*function len(arr, pos){
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
s;*/

// ----- Fibonacci -----
/*var x = 4;
function fibonacci(num){
    if(num == 0){
    	return 0;
    }
    if(num == 1){
        return 1;
    }
    return (fibonacci(num-1))+(fibonacci(num-2));
}
fibonacci(x);*/

// ----- Primos -----
/*n = [];

for(var i = 2; i < 100; i++){
	n = n.concat([true]);
}

primos = [];
for(var i = 2; i < 100; i++){
	if(n[i]){
		primos = primos.concat([i])
		for(var j = 2; j*i < 100; j++){
			n[j*i] = false;				
		}
	}
}

primos;*/

// ----- Soma Array -----
/*function len(arr, pos){
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
somaArray(x);*/

// ----- Dia da Semana -----
/*function diaSemana(a){
  switch(a){
    case 1:
      return "Domingo";
    case 2:
      return "Segunda-Feira";
    case 3:
      return "Terça-Feira";
    case 4:
      return "Quarta-feira";
    case 5:
      return "Quinta-feira";
    case 6:
      return "Sexta-feira";
    case 7:
      return "Sábado";
    default:
      return "Dia inválido";
  }
}

var a = 3;
var x = diaSemana(a);
x;*/

// ----- Palindromo -----
/*function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}

function palindrome(a){
  var b = [];
  var i = 0;
  for (i; i<len(a,0); i++){
    b = b.concat(a[len(a,0) - i - 1]);
  }

  return b.equals(a);
}

var a = [5,7,5];
var x = palindrome(a);
x;
*/

// ----- Danilo -----
/*var a = 20;

function teste(){
	var b = a + 50;
	return (b+30);
}

var m = teste();
m;
*/

