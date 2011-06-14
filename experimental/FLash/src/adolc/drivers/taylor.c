/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     drivers/taylor.c
 Revision: $Id: taylor.c 134 2009-03-03 14:25:24Z imosqueira $
 Contents: Easy to use drivers for the evaluation of higher order derivative
           tensors and inverse/impicit function differentiation
 
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History:
          20040416 kowarz:       adapted to configure - make - make install
          19990216 olvo/andrea:  mindec in tensor_eval geklammert
          19981214 olvo/walther: ec Xhelp
          19981130 olvo:         last check (includes ...)
          19981120 olvo/walther: return values
          19980914 olvo:         Jac_solv --> jac_solv
          19980814 olvo:         integral constant expressions in
                                 arrays (ANSI-C) 
          19980806 walther:      (1) access to tensors 
                                 (2) seems to be finished 
          19980804 olvo/walther: debugging --> finished?
 
----------------------------------------------------------------------------*/
#include "drivers/taylor.h"
#include "interfaces.h"
#include "adalloc.h"

#include <math.h>

BEGIN_C_DECLS

/****************************************************************************/
/*                                                              STRUCT ITEM */
struct item {
    int a;                 /* address in array of derivatives */
    int b;                 /* absolute value of the correspondig multiindex i */
    double c;              /* value of the coefficient c_{i,j} */
    struct item *next;     /* next item */
};


/****************************************************************************/
/*                                                     DEALLOCATE COEFFLIST */
void freecoefflist( int dim, struct item *coeff_list ) {
    int i;
    struct item *ptr1;
    struct item *ptr2;

    for (i=0; i<dim; i++)  /* sum over all multiindices jm with |jm| = d */
    { ptr1 = &coeff_list[i];
        ptr1 = ptr1->next;
        while (ptr1 != NULL) {
            ptr2 = ptr1->next;
            free((char *) ptr1);
            ptr1 = ptr2;
        }
    }
}


/****************************************************************************/
/*                                            ALLOCATE/DEALLOCATE COEFFLIST */
double* tensoriglob;

/*--------------------------------------------------------------------------*/
/* Allcoate space for symmetric derivative tensors
   of up to order d in n variables, derivatives are  */
void* tensorpoint( int n, int d ) {
    int i;
    void* t;

    if (d == 1) {
        t = (void*) tensoriglob;
        tensoriglob += n+1;
    } else {
        t = (void*) malloc((n+1)*sizeof(void*));
        for (i=0; i<=n; i++)
            ((void**)t)[i] = (void*) tensorpoint(i,d-1);
    }
    return t;
}

/*--------------------------------------------------------------------------*/
void** tensorsetup( int m, int n, int d, double** tensorig ) {
    int i;
    void** t = (void**) malloc(m*sizeof(void*));

    for (i=0; i<m; i++) {
        tensoriglob = tensorig[i];
        t[i] = (void*) tensorpoint(n,d);
    }
    return t;
}

/*--------------------------------------------------------------------------*/
/* Deallocate space for symmetric derivative tensors
   of up to order d in n variables  */
void freetensorpoint( int n, int d, double** tensor ) {
    int i;
    double* t;

    if (d > 2)
        for(i=0;i<=n;i++) {
            t = tensor[i];
            freetensorpoint(i,d-1,(double **) t);
            free((char *) t);
        }
}

/*--------------------------------------------------------------------------*/
void freetensor( int m, int n, int d, double** tensor ) {
    int i;
    double* t;

    for (i=0; i<m; i++) {
        t = tensor[i];
        freetensorpoint(n,d,(double **)t);
        free((char *) t);
    }
}


/****************************************************************************/
/*                                                           SOME UTILITIES */

long binomi( int a, int b ) {
    int i;
    long result = 1;

    for (i=1; i<=b; i++)
        result = (result*(a-i+1))/i; /* olvo 980804 () */
    return result;
};

/*--------------------------------------------------------------------------*/
double dbinomi( double a, int b ) {
    int i;
    double result = 1.0;

    for (i=1; i<=b; i++)
        result = result*(a-i+1)/i;
    return result;
};

/*--------------------------------------------------------------------------*/
double summand(int p, int d, int* jm, int* km, int order_im, int order_km, long binomiZ) {   /* calculates summation value for fixed j, i, k with terms used in the article.*/
    int i;
    double result, order_k_by_d;

    order_k_by_d = order_km/(double)d;
    result = 1.0;
    for (i=0; i<order_im; i++) result *= order_k_by_d;     /* (|k|/d)^i */
    if ((order_im+order_km)%2==1) result *= -1.0;             /* (-1)^(|i-k|) */
    result *= binomiZ;
    for (i=0; i<p; i++) result *= dbinomi(d*km[i]/(double)order_km, jm[i]);
    return result;
};

/****************************************************************************/
/*                                                    EVALUATE COEFFICIENTS */

void coeff(int p, int d, struct item* coeff_list) {
    int i, j, u, n, index_coeff_list, order_im, order_km, address;
    int* jm = (int*) malloc(p*sizeof(int));     /* Multiindex j */
    int* im = (int*) malloc(p*sizeof(int));     /* Multiindex i */
    int* km = (int*) malloc(p*sizeof(int));     /* Multiindex k */
    struct item* ptr;
    double sum;
    long binomiZ;           /* whole number binomial coefficient */

    jm[0] = d;
    for (i=1; i<p; i++) jm[i] = im[i] = 0;
    for (i=0; i<p; i++) km[i] = 0;
    order_km = 0;

    for (index_coeff_list = 0; 1; index_coeff_list++) {  /* travers coeff_list, i.e. create all j with |j| = d. */
        ptr = NULL;
        for (order_im=1; order_im<=d; order_im++) {  /* travers all orders from 1 to d for i */
            im[p-1]=0;
            im[0] = order_im;
            while (1) {   /* create all i with order order_im. */
                sum = 0;
                binomiZ = 1;
                for (i=0; i<p; i++) /* check, whether i valid. */
                    if ((jm[i]>0)&&(im[i]==0)) break;
                if (i==p)
                    while (1) {   /* create all k where 0<k<=i */
                        for (i=p-1; i>=0; i--)
                            if (km[i]<im[i]) {
                                km[i]++;
                                order_km++;
                                binomiZ *= im[i]-km[i]+1;  /* for (i over k)/(i over k-1) = (i-k+1)/k */
                                binomiZ /= km[i];
                                break;
                            } else {
                                /* binomiZ doesn't change, for (i over k) = 1 if k=0 and also if k=i */
                                order_km -= km[i];
                                km[i] = 0;
                            };
                        if (i==-1) break;

                        sum += summand(p,d,jm,km,order_im,order_km,binomiZ);
                    };

                if (sum!=0) { /* Store coefficient */
                    if (ptr==NULL)
                        ptr = &coeff_list[index_coeff_list];
                    else {
                        ptr->next = (struct item*) malloc(sizeof(struct item));
                        ptr = ptr->next;
                    };

                    address = 0; /* calculate address for ptr->a */
                    j = d-order_im+1;
                    for (u=0; u<p; u++)  /* It is sum(binomial(i+k,j+k),k=0..n-1) = */
                        if (im[u]!=0)      /* = ((j+n)*binomial(i+n,j+n)-j*binomial(i,j))/(1+i-j) */
                        {
                            i = u+j;
                            n = im[u];
                            address += ((j+n)*binomi(i+n,j+n)-j*binomi(i,j))/(1+i-j);
                            j += n;
                        };
                    ptr->a = address;
                    ptr->b = order_im;
                    ptr->c = sum;
                };

                if ((im[p-1]==order_im)||(p==1)) break;
                for (i=p-2; im[i]==0; i--); /* find first nonvanishing entry on the right. */
                im[i]--;
                im[i+1] = im[p-1]+1;
                if (i!=p-2) im[p-1] = 0;
            };
        };

        ptr->next = NULL; /* mark end of queue. */

        if ((jm[p-1]==d)||(p==1)) break;
        for (i=p-2; jm[i]==0; i--); /* find first nonvanishing entry on the right. */
        jm[i]--;
        jm[i+1] = jm[p-1]+1;
        if (i!=p-2) jm[p-1] = 0;
    };

    free((char*) jm);
    free((char*) im);
    free((char*) km);
};


/*--------------------------------------------------------------------------*/
void convert( int p, int d, int *im, int *multi ) {
    int i;

    for (i=0; i<p; i++)
        multi[i] = 0;
    for (i=0; i<d; i++)
        if (im[i]) /* olvo/walther 980804 new tl */
            multi[im[i]-1] += 1;
}

/*--------------------------------------------------------------------------*/
int address( int d, int* im ) {
    int i,
    add = 0;

    for (i=0; i<d; i++)
        add += binomi(im[i]+i,i+1);
    return add;
}



/****************************************************************************/
/*                                                           MORE UTILITIES */

/*--------------------------------------------------------------------------*/
void multma3vec2( int n, int p, int d, int bd,
                  double ***X, double **S, int **jm ) {
    int i,j,k;
    double sum;

    for (i=0; i<n; i++)
        for (k=0; k<bd; k++) {
            sum = 0;
            for (j=0; j<p; j++)
                sum += S[i][j]*jm[k][j];
            X[i][k][0] = sum;
            for (j=1; j<d; j++)
                X[i][k][j] = 0;
        }
}

/*--------------------------------------------------------------------------*/
void multma2vec2( int n, int p, int bd, double **X, double **S, int **jm ) {
    int i,j,k;
    double sum;

    for (i=0; i<n; i++)
        for (k=0; k<bd; k++) {
            sum = 0;
            for (j=0; j<p; j++)
                sum += S[i][j]*jm[k][j];
            X[i][k] = sum;
        }
}

/*--------------------------------------------------------------------------*/
void multma2vec1( int n, int p, int d, double **X, double **S, int *jm ) {
    int i,j;
    double sum;

    for (i=0; i<n; i++) {
        sum = 0;
        for (j=0; j<p; j++)
            sum += S[i][j]*jm[j];
        X[i][1] = sum;
        for (j=2; j<d; j++)
            X[i][j] = 0;
    }
}


/****************************************************************************/
/*----------------------------------------------------------------------------
  From:
  Olaf Vogel 
  Institute of Scientific Computing
  TU Dresden
  diploma thesis "Zur Berechnung von Rand und Randfaltungen"
  File:    dvGausz.C
  LU-Factorisation and triangular backward/forward substitution
   
----------------------------------------------------------------------------*/

/* test if zero */
#define ZERO 1.0E-15

/*--------------------------------------------------------------------------*/
int LUFactorization( double** J, int n, int* RI, int* CI ) {
    int i, j, k, cIdx, rIdx, h;
    double v;

    for (i=0; i<n; i++)
        RI[i]=i;
    for (j=0; j<n; j++)
        CI[j]=j;
    /* n Gausz-steps with full Pivoting */
    for (k=0; k<n; k++) {
        v=0.0;
        cIdx=rIdx=0;
        /* Pivotsearch */
        for (i=k; i<n; i++)
            for (j=k; j<n; j++)
                if (fabs(J[RI[i]][CI[j]])>v) {
                    v=fabs(J[RI[i]][CI[j]]);
                    rIdx=i;
                    cIdx=j;
                }
        if (ZERO > v) {
            fprintf(DIAG_OUT,
                    "Error:LUFactorisation(..): no Pivot in step %d (%le)\n",k+1,v);
            return -(k+1);
        }
        /* row and column change resp. */
        if (rIdx > k) {
            h=RI[k];
            RI[k]=RI[rIdx];
            RI[rIdx]=h;
        }
        if (cIdx > k) {
            h=CI[k];
            CI[k]=CI[cIdx];
            CI[cIdx]=h;
        }
        /* Factorisation step */
        for (i=k+1; i<n; i++) { /* L-part */
            J[RI[i]][CI[k]]/=J[RI[k]][CI[k]];
            /* R-part */
            for (j=k+1; j<n; j++)
                J[RI[i]][CI[j]]-=J[RI[i]][CI[k]]*J[RI[k]][CI[j]];
        }
    }
    return k;
}

/*--------------------------------------------------------------------------*/
void GauszSolve( double** J, int n, int* RI, int* CI, double* b ) {
    double* tmpZ;
    int i,j;

    tmpZ = myalloc1(n);
    for (i=0; i<n; i++) {
        tmpZ[i]=b[RI[i]];
        for (j=0; j<i; j++)
            tmpZ[i]-=J[RI[i]][CI[j]]*tmpZ[j];
    }
    for (i=n-1; i>=0; i--) {
        b[CI[i]]=tmpZ[i];
        for (j=i+1; j<n; j++)
            b[CI[i]]-=J[RI[i]][CI[j]]*b[CI[j]];
        b[CI[i]]/=J[RI[i]][CI[i]];
    }
    free(tmpZ);
}


/****************************************************************************/
int jac_solv( unsigned short tag, int n, double* x, double* b,
              unsigned short sparse, unsigned short mode ) {
    static double **J;
    static double **I;
    static double *y;
    static double *xold;
    static int* ri;
    static int* ci;
    static int nax,tagold,modeold,cgd;
    int i,j;
    int rc = 3;

    if ((n != nax) || (tag != tagold)) {
        if (nax) {
            free(*J);
            free(J);
            free(*I);
            free(I);
            free(xold);
            free(ri);
            free(ci);
            free(y);
        }
        J = myalloc2(n,n);
        I = myalloc2(n,n);
        y = myalloc1(n);

        xold = myalloc1(n);
        ri = (int*)malloc(n*sizeof(int));
        ci = (int*)malloc(n*sizeof(int));
        for (i=0; i<n; i++) {
            xold[i] = 0;
            for (j=0;j<n;j++)
                I[i][j]=(i==j)?1.0:0.0;
        }
        cgd = 1;
        modeold = 0;
        nax = n;
        tagold = tag;
    }
    if (cgd == 0)
        for (i=0; i<n; i++)
            if (x[i] != xold[i])
                cgd = 1;
    if (cgd == 1)
        for (i=0; i<n; i++)
            xold[i] = x[i];
    switch(mode) {
    case 0:
        MINDEC(rc,zos_forward(tag,n,n,1,x,y));
        MINDEC(rc,fov_reverse(tag,n,n,n,I,J));
        break;
    case 1:
        if ((modeold == 0) || (cgd == 1)) {
            MINDEC(rc,zos_forward(tag,n,n,1,x,y));
            MINDEC(rc,fov_reverse(tag,n,n,n,I,J));
        }
        if (LUFactorization(J,n,ri,ci) < 0)
            return -3;
        modeold = 1;
        break;
    case 2:
        if ((modeold < 1) || (cgd == 1)) {
            MINDEC(rc,zos_forward(tag,n,n,1,x,y));
            MINDEC(rc,fov_reverse(tag,n,n,n,I,J));
            if (LUFactorization(J,n,ri,ci) < 0)
                return -3;
        }
        GauszSolve(J,n,ri,ci,b);
        modeold = 2;
        break;
    }
    cgd = 0;
    return rc;
}


/****************************************************************************/
int inverse_Taylor_prop( unsigned short tag, int n, int d,
                         double** Y, double** X ) {
    int i,j,l,q;
    static double **I;
    register double bi;
    static double** Xhelp;
    static double** W;
    static double* xold;
    static double ***A;
    static double *w;
    static int *dd;
    static double *b;
    static int nax,dax,bd,cgd;
    static short **nonzero;
    short* nz;
    double* Aij;
    double* Xj;
    int ii, di, da, Di;
    int rc = 3;

    /* Re/Allocation Stuff */
    if ((n != nax) || (d != dax)) {
        if (nax) {
            free(**A);
            free(*A);
            free(A);
            free(*I);
            free(I);
            free(*W);
            free(W);
            free(*Xhelp);
            free(Xhelp);
            free(w);
            free(xold);
            free(*nonzero);
            free(nonzero);
            free(dd);
            free(b);
        }
        A = myalloc3(n,n,d+1);
        I = myalloc2(n,n);
        W = myalloc2(n,d);
        Xhelp = myalloc2(n,d);
        w = myalloc1(n);
        dd = (int*)malloc((d+1)*sizeof(int));
        b  = (double*)malloc(n*sizeof(double));
        xold = (double*)malloc(n*sizeof(double));
        nonzero = (short**)malloc(n*sizeof(short*));
        nz = (short*)malloc(n*n*sizeof(short));
        for (i=0; i<n; i++) {
            nonzero[i] = nz;
            nz = nz + n;
            xold[i] = 0;
            for (j=0; j<n; j++)
                I[i][j]=(i==j)?1.0:0.0;
        }
        cgd = 1;
        nax=n;
        dax=d;
        dd[0] = d+1;
        i = -1;
        while(dd[++i] > 1)
            dd[i+1] = (int)ceil(dd[i]*0.5);
        bd = i+1;
    }
    if (cgd == 0)
        for (i=0; i<n; i++)
            if (X[i][0] != xold[i])
                cgd = 1;
    if (cgd == 1) {
        cgd = 0;
        for (i=0; i<n; i++)
            xold[i] = X[i][0];
        MINDEC(rc,jac_solv(tag,n,xold,b,0,1));
        if (rc == -3)
            return -3;
    }
    ii = bd;
    for (i=0; i<n; i++)
        for (j=0; j<d; j++)
            Xhelp[i][j] = X[i][j+1];

    while (--ii > 0) {
        di = dd[ii-1]-1;
        Di = dd[ii-1]-dd[ii]-1;
        MINDEC(rc,hos_forward(tag,n,n,di,Di+1,xold,Xhelp,w,W));
        MINDEC(rc,hov_reverse(tag,n,n,Di,n,I,A,nonzero));
        da = dd[ii];
        for (l=da; l<dd[ii-1]; l++) {
            for (i=0; i<n; i++) {
                if (l == 0)
                    bi = w[i]-Y[i][0];
                else
                    bi = W[i][l-1]-Y[i][l];
                for (j=0; j<n; j++)
                    if (nonzero[i][j]>1) {
                        Aij = A[i][j];
                        Xj = X[j]+l;
                        for (q=da; q<l; q++)
                            bi += (*(++Aij))*(*(--Xj));
                    }
                b[i] = -bi;
            }
            MINDEC(rc,jac_solv(tag,n,xold,b,0,2));
            if (rc == -3)
                return -3;
            for (i=0; i<n; i++) {
                X[i][l] += b[i];
                /* 981214 new nl */
                Xhelp[i][l-1] += b[i];
            }
        }
    }
    return rc;
}


/****************************************************************************/
int inverse_tensor_eval( int tag, int n, int d, int p,
                         double *x, double **tensor, double** S ) {
    static int dim;
    static int dold,pold;
    static struct item *coeff_list;
    int i,j,dimten;
    int *it = (int*) malloc(d*sizeof(int));
    double** X;
    double** Y;
    int *jm;
    double *y = (double*) malloc(n*sizeof(double));
    struct item *ptr;
    int rc = 3;

    dimten=binomi(p+d,d);
    for(i=0;i<n;i++)
        for(j=0;j<dimten;j++)
            tensor[i][j] = 0;
    MINDEC(rc,zos_forward(1,n,n,0,x,y));
    if (d > 0) {
        if ((d != dold) || (p != pold)) {
            if (pold) { /* olvo 980728 */
                dim = binomi(pold+dold-1,dold);
                freecoefflist(dim,coeff_list);
                free((char*) coeff_list);
            }
            dim = binomi(p+d-1,d);
            coeff_list = (struct item *) malloc(sizeof(struct item)*dim);
            coeff(p,d, coeff_list);
            dold = d;
            pold = p;
        }
        jm = (int *)malloc(sizeof(int)*p);
        X = myalloc2(n,d+1);
        Y = myalloc2(n,d+1);
        for (i=0; i<n; i++) {
            X[i][0] = x[i];
            for (j=1; j<d; j++)
                X[i][j] = 0;
            Y[i][0] = y[i];
        }
        if (d == 1) {
            it[0] = 0;
            for (i=0; i<dim; i++)  /* sum over all multiindices jm with |jm| = d */
            { it[0] = it[0]+1;
                convert(p,d,it,jm);
                ptr = &coeff_list[i];
                multma2vec1(n,p,d,Y,S,jm);
                MINDEC(rc,inverse_Taylor_prop(tag,n,d,Y,X));
                if (rc == -3)
                    return -3;
                do {
                    for(j=0;j<n;j++)
                        tensor[j][ptr->a] += X[j][ptr->b]*ptr->c;
                    ptr = ptr->next;
                } while (ptr != NULL);
            }
        } else {
            for (i=0; i<d-1; i++)
                it[i] = 1;
            it[d-1] = 0;
            for (i=0; i<dim; i++)  /* sum over all multiindices jm with |jm| = d */
            { it[d-1] = it[d-1]+1;
                for (j=d-2; j>=0; j--)
                    it[j] = it[j] + it[j+1]/(p+1);
                for (j=1; j<d; j++)
                    if (it[j] > p) it[j] = it[j-1];
                convert(p,d,it,jm);
                multma2vec1(n,p,d,Y,S,jm); /* Store S*jm in Y */
                MINDEC(rc,inverse_Taylor_prop(tag,n,d,Y,X));
                if (rc == -3)
                    return -3;
                ptr = &coeff_list[i];
                do {
                    for(j=0;j<n;j++)
                        tensor[j][ptr->a] += X[j][ptr->b]*ptr->c;
                    ptr = ptr->next;
                } while (ptr != NULL);
            }
        }
        free((char*) jm);
        free((char*) *X);
        free((char*) X);
        free((char*) *Y);
        free((char*) Y);
    }
    for(i=0;i<n;i++)
        tensor[i][0] = x[i];
    free((char*) y);
    free((char*) it);
    return rc;
}


/****************************************************************************/
int tensor_eval( int tag, int m, int n, int d, int p,
                 double* x, double **tensor, double **S ) {
    static int bd,dim;
    static int dold,pold;
    static struct item *coeff_list;
    int i,j,k,dimten,ctr;
    int **jm, jmbd=0;
    int *it = (int*) malloc(d*sizeof(int));
    double *y = (double*) malloc(m*sizeof(double));
    double*** X;
    double*** Y;
    struct item *ptr[10];
    int rc = 3;

    dimten=binomi(p+d,d);
    for (i=0; i<m; i++)
        for (j=0; j<dimten; j++)
            tensor[i][j] = 0;
    if (d == 0) {
        MINDEC(rc,zos_forward(1,m,n,0,x,y));
    } else {
        if ((d != dold) || (p != pold)) {
            if (pold) {
                dim = binomi(pold+dold-1,dold);
                freecoefflist(dim,coeff_list);
                free((char*) coeff_list);
            }
            dim = binomi(p+d-1,d);
            if (dim < 10)
                bd = dim;
            else
                bd = 10;
            coeff_list = (struct item *) malloc(sizeof(struct item)*dim);
            coeff(p,d, coeff_list);
            dold = d;
            pold = p;
        }
        jmbd = bd;
        jm = (int **) malloc(jmbd*sizeof(int*));
        for (i=0; i<jmbd; i++)
            jm[i] = (int *) malloc(p*sizeof(int));
        if (d == 1) {
            X = myalloc3(1,n,bd);
            Y = myalloc3(1,m,bd);
            ctr   = 0;
            it[0] = 0;
            for (i=0; i<dim; i++) /* sum over all multiindices jm with |jm| = d */
            { it[0] = it[0]+1;
                convert(p,d,it,jm[ctr]);
                ptr[ctr] = &coeff_list[i];
                if (ctr < bd-1)
                    ctr += 1;
                else {
                    multma2vec2(n,p,bd,X[0],S,jm);
                    MINDEC(rc,fov_forward(tag,m,n,bd,x,X[0],y,Y[0]));
                    for (k=0; k<bd; k++)
                        do {
                            for (j=0; j<m; j++)
                                tensor[j][ptr[k]->a] += Y[0][j][k]*ptr[k]->c;
                            ptr[k] = ptr[k]->next;
                        } while (ptr[k] != NULL);
                    if (dim-i < bd)
                        bd = dim-i-1;
                    ctr = 0;
                }
            }
        } else {
            X = myalloc3(n,bd,d);
            Y = myalloc3(m,bd,d);
            ctr = 0;
            for (i=0; i<d-1; i++)
                it[i] = 1;
            it[d-1] = 0;
            for (i=0; i<dim; i++) /* sum over all multiindices jm with |jm| = d */
            { it[d-1] = it[d-1]+1;
                for (j=d-2; j>=0; j--)
                    it[j] = it[j] + it[j+1]/(p+1);
                for (j=1; j<d; j++)
                    if (it[j] > p)
                        it[j] = it[j-1];
                convert(p,d,it,jm[ctr]);
                ptr[ctr] = &coeff_list[i];
                if (ctr < bd-1)
                    ctr += 1;
                else {
                    multma3vec2(n,p,d,bd,X,S,jm);
                    MINDEC(rc,hov_forward(tag,m,n,d,bd,x,X,y,Y));
                    for (k=0; k<bd; k++)
                        do {
                            for (j=0; j<m; j++)
                                tensor[j][ptr[k]->a] += Y[j][k][ptr[k]->b-1]*ptr[k]->c;
                            ptr[k] = ptr[k]->next;
                        } while (ptr[k] != NULL);
                    if (dim-i < bd)
                        bd = dim-i-1;
                    ctr = 0;
                }
            }
        }
        for (i=0; i<jmbd; i++)
            free((char*) *(jm+i));
        free((char*) jm);
        free((char*) **X);
        free((char*) *X);
        free((char*) X);
        free((char*) **Y);
        free((char*) *Y);
        free((char*) Y);
    }
    for(i=0;i<m;i++)
        tensor[i][0] = y[i];
    bd = jmbd;
    free((char*) y);
    free((char*) it);
    return rc;
}


/****************************************************************************/
void tensor_value( int d, int m, double *y, double **tensor, int *multi ) {
    int i, j, max, ind, add;
    int *im = (int*) malloc(d*sizeof(int));

    max = 0;
    ind = d-1;
    for (i=0; i<d; i++) {
        if (multi[i] > max)
            max = multi[i];
        im[i] = 0;
    }
    for (i=0; i<d; i++) {
        if (multi[i] == max)  /* olvo 980728 == instead of = */
        { im[ind] = multi[i];
            multi[i] = 0;
            max = 0;
            ind -= 1;
            for (j=0; j<d; j++)
                if (multi[j] > max)
                    max = multi[j];
        }
    }
    add = address(d,im);
    for (i=0; i<m; i++)
        y[i] = tensor[i][add];
    free((char*) im);
}

END_C_DECLS





















