#define lambda0(body) ({body})
#define lambda1(x,body) ({ \
	typeof(lmbd_arg1) x = lmbd_arg1; \
	lambda0(body);})

#define lambda2(x,y,body) ({\
	typeof(lmbd_arg1) x = lmbd_arg1; \
	typeof(lmbd_arg2) y = lmbd_arg2; \
	lambda0(body);})


#define lambda1_call(l,x) ({\
	typeof(x) lmbd_arg1 = x; \
	l;})

#define lambda2_call(l,x,y) ({\
	typeof(x) lmbd_arg1 = x; \
	typeof(y) lmbd_arg2 = y; \
	l;})

#define forlist(list,action) ({\
	int p; \
	for(p = list; p; p = cdr(p)) \
		lambda1_call(action, car(p)); \
	})


#define macr ({
#define endm })

