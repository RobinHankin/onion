/*basic octonion product: multiplies two octonions */
static void oct_prod_single(const double *x, const double *y, double *out)
{
/*Re */	out[0] = +x[0]*y[0] -x[1]*y[1] -x[2]*y[2] -x[3]*y[3] -x[4]*y[4] -x[5]*y[5] -x[6]*y[6] -x[7]*y[7];
/*Oi */	out[1] = +x[1]*y[0] +x[0]*y[1] -x[3]*y[2] +x[2]*y[3] -x[5]*y[4] +x[4]*y[5] +x[7]*y[6] -x[6]*y[7];
/*Oj */	out[2] = +x[2]*y[0] +x[3]*y[1] +x[0]*y[2] -x[1]*y[3] -x[6]*y[4] -x[7]*y[5] +x[4]*y[6] +x[5]*y[7];
/*Ok */	out[3] = +x[3]*y[0] -x[2]*y[1] +x[1]*y[2] +x[0]*y[3] -x[7]*y[4] +x[6]*y[5] -x[5]*y[6] +x[4]*y[7];
/*Ol */	out[4] = +x[4]*y[0] +x[5]*y[1] +x[6]*y[2] +x[7]*y[3] +x[0]*y[4] -x[1]*y[5] -x[2]*y[6] -x[3]*y[7];
/*Oil*/	out[5] = +x[5]*y[0] -x[4]*y[1] +x[7]*y[2] -x[6]*y[3] +x[1]*y[4] +x[0]*y[5] +x[3]*y[6] -x[2]*y[7];
/*Ojl*/	out[6] = +x[6]*y[0] -x[7]*y[1] -x[4]*y[2] +x[5]*y[3] +x[2]*y[4] -x[3]*y[5] +x[0]*y[6] +x[1]*y[7];
/*Okl*/	out[7] = +x[7]*y[0] +x[6]*y[1] -x[5]*y[2] -x[4]*y[3] +x[3]*y[4] +x[2]*y[5] -x[1]*y[6] +x[0]*y[7];
}  

/* vectorized version: takes two arrays of length a multiple of 8 and interprets them
as octonions one after the other.
*/
void octonion_prod(const double *x, const double *y, const int *n, double *out)
{
	int i;
	const int nn = (*n);
	for(i=0 ; i < nn ; i+=8){
		oct_prod_single(x+i, y+i, out+i);
	}
}

 
/*basic quaternion product: multiplies two quaternions */
static void quat_prod_single(const double *x, const double *y, double *out)
{
	out[0] = +x[0]*y[0] -x[1]*y[1] -x[2]*y[2] -x[3]*y[3];
	out[1] = +x[1]*y[0] +x[0]*y[1] -x[3]*y[2] +x[2]*y[3];
	out[2] = +x[2]*y[0] +x[3]*y[1] +x[0]*y[2] -x[1]*y[3];
	out[3] = +x[3]*y[0] -x[2]*y[1] +x[1]*y[2] +x[0]*y[3];
}  

/* vectorized version: takes two arrays of length a multiple of 4 and interprets them
as quaternions one after the other.
*/
void quaternion_prod(const double *x, const double *y, const int *n, double *out)
{
	int i;
	const int nn = (*n);
	for(i=0 ; i < nn ; i+=4){
		quat_prod_single(x+i, y+i, out+i);
	}
}
 
