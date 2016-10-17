//var x = 10, y = 10/*, z = "asd"*/;
/*function soma(x,y){
	return x+y;
}
var mult = function(x, y){
	return x*y;
}
var sub = function(x,y){
	return x-y;
}
var ola = function a(){
	return 1;
}
function fat(x){
	if(x == 1) return 1;
	return x*fat(x-1);
}
function bla(x){
	if(x == 1) return 1;
	var oi = x;
	return bla(x-1)*(oi+x);
}*/
/*z;
if(x == 10){
	x = 1;
}
else{
	x = 100;
	y = x;
}
x + y;
while(x < 200){
	x = x + 1;
}
x;
do{
	x = x - 1;
}while(x > 50);
x;
return 4;*/
//teste
/*
uahsiduhaosudhoashdoajsodih
aousdh
aishd
uosdahihasd
*/
//x;
//var a = [1,2,3];
//for(;x<2;x=x+1){}
//for(var a = 0;a<10;a++){}
//if(x == 10) var y = 1000;
//y++;
//++y;
//y--;
//--y;
//(y == 100) ? (x = 20) : (x = 200)
//x = mult(x,y);
//y = fat(5);
//y = bla(5);
/*switch(x){
	case 10:
		y = 20;
		x = 30;
	case 30:
		y = 60;
	default:
		y = 109312;
}*/
/*for(x = 1;x<10;x=x+1){
	var y = 1000;
}*/
//var z = [1,2,3,4]
//x = z[1];
//x;
//var a = soma(2,3);
function len(arr, pos){
	if(arr[pos] == null) return pos;
	return len(arr,(pos+1));
}
var a = [1,2,3,4,5,6];
len(a, 0);
function floorf(num){
	return (num%1);
}
var x = 1;
var a1 = 10.5 + 1.5;
//var a1 = 10.5%1;
//var a2 = floorf(-1.5);
