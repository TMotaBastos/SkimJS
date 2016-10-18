n = [];

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

primos;