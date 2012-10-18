/*----------------------------------------------------------------------------
 ADOL-C -- Automatic Differentiation by Overloading in C++
 File:     avector.h
 Revision: $Id: avector.h 134 2009-03-03 14:25:24Z imosqueira $
 Contents: Avector.h defines classes of vectors and matrices.
           badoublev  --> class of basic active vectors. 
           adubv      --> class of temporary active vectors.
                      (derived from badoublev.  Contains copy constructors, 
                      destructors.)
           adoublev   --> class of active vectors. (derived from badoublev,
                      contains the standard constructors and destructors.
                  
 Copyright (c) 2004
               Technical University Dresden
               Department of Mathematics
               Institute of Scientific Computing
  
 This file is part of ADOL-C. This software is provided under the terms of
 the Common Public License. Any use, reproduction, or distribution of the
 software constitutes recipient's acceptance of the terms of this license.
 See the accompanying copy of the Common Public License for more details.
 
 History:
          20041110 kowarz: ifdefs for tapeless forward added
          20040423 kowarz: adapted configure - make - make install
          19981130 olvo:   last check (includes ...)
	                        NOTICE: I think everything concerning vectors 
                                   has to be checked again in detail!
 
----------------------------------------------------------------------------*/

#if !defined(ADOLC_AVECTOR_H)
#define ADOLC_AVECTOR_H 1

#if !defined(ADOLC_TAPELESS)

#include "common.h"

/****************************************************************************/
/*                                                         THIS FILE IS C++ */
#ifdef __cplusplus

/****************************************************************************/
/*                                                     FORWARD DECLARATIONS */
class badoublev;
class adoublev;
class adubv;
/* class doublev; removed 1/95 */
class err_retu;
class asubv;


/****************************************************************************/
/*                                                          ANY ERROR CLASS */
class ADOLC_DLL_EXPORT err_retu {
    const char* message;
public:
    err_retu(const char* x) {
        printf("%s \n",x);
    };
};


/****************************************************************************/
/*                                            DECLARATION OF VECTOR CLASSES */

/* Passive vectors and matrices were REMOVED 1/95  */


/****************************************************************************/
/*                                                          CLASS BADOUBLEV */
class ADOLC_DLL_EXPORT badoublev {
protected:
    locint start_loc;  /* Starting location of vector in store */
    int size;          /* Size of the vector */
    badoublev() {};
    badoublev(int lo, int sz) {
        start_loc = lo;
        size=sz;
    };
    badoublev(const badoublev& a) {
        start_loc = a.start_loc;
        size=a.size;
    };

public:

    /* Access functions */
    int sz() const {
        return size;
    }  /* Get the size of the vector */
    locint loc() const {
        return start_loc;
    }  /* Get the size of the vector */

    asub operator[](const along&) const;

    /* excluded before 1/95
      badoublev& operator >>= (doublev& );
      badoublev& operator <<= (doublev& );
      badoublev& operator >>= (double* );
      badoublev& operator <<= (double* );
    */

    badouble operator[](int) const;  /* Can access component like an array */

    badoublev& operator+=(const badoublev&);
    badoublev& operator-=(const badoublev&);
    badoublev& operator*=(double);
    badoublev& operator/=(double);
    /* removed 1/95
      badoublev& operator-=(const doublev&);
      badoublev& operator+=(const doublev&);
    */
    /* removed Sep/01/96
      badoublev& operator-=(double*);
      badoublev& operator+=(double*);
    */
    badoublev& operator*=(const badouble& );
    badoublev& operator/=(const badouble& );
    ADOLC_DLL_EXPORT friend adubv operator/(const badoublev &op1, const badouble &n);
    inline ADOLC_DLL_EXPORT friend adubv operator/(const badoublev &op1, double n);
    /*  removed 1/95
      badoublev& operator= (const doublev&);
    */
    badoublev& operator= (const badoublev&);
    badoublev& operator= (const adubv &y);
    badoublev& operator= (const adoublev &y);

    ADOLC_DLL_EXPORT friend std::ostream& operator << (std::ostream&, const badoublev&);

    ADOLC_DLL_EXPORT friend adubv operator+ (const badoublev &x);
    ADOLC_DLL_EXPORT friend adubv operator- (const badoublev &x);

    /* overload operations */
    ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1,const badoublev &op2);
    ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1,const badoublev &op2);
    ADOLC_DLL_EXPORT friend adubv operator*(const badoublev &op1, double n);
    ADOLC_DLL_EXPORT friend adubv operator*(double n, const badoublev &op1);
    ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, const badoublev &op2);

    /* overloaded for interaction of constant and active vectors */
    /* removed 1/95
      ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1, const doublev &op2);
      ADOLC_DLL_EXPORT friend adubv operator+(const doublev &op1, const badoublev &op2);
    */
    ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1, double* op2);
    ADOLC_DLL_EXPORT friend adubv operator+(double* op2, const badoublev &op1);
    /* removed 1/95
      ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1, const doublev &op2);
      ADOLC_DLL_EXPORT friend adubv operator-(const doublev &op1, const badoublev &op2);
    */
    ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1, double* op2);
    ADOLC_DLL_EXPORT friend adubv operator-(double* op1, const badoublev &op2);
    /* removed 1/95
      ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, const doublev &op2);
      ADOLC_DLL_EXPORT friend adub operator*(const doublev &op1, const badoublev &op2);
    */
    ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, double* op2);
    ADOLC_DLL_EXPORT friend adub operator*(double* op1, const badoublev &op2);

    /* overloaded for interaction of active scalars and active vectors */
    /* removed 1/95
      ADOLC_DLL_EXPORT friend adubv operator/(const doublev &op1, const badouble &n);
    */
    ADOLC_DLL_EXPORT friend adubv operator*(const badoublev &op1, const badouble &n);
    ADOLC_DLL_EXPORT friend adubv operator*(const badouble &n, const badoublev &op1);
    /* excluded operations */
    err_retu operator>>=(double op1) {
        /*     double x=op1;*/
        return("ADOL-C error: illegal argument combination for operator >>=\n");
    };
    err_retu operator<<=(double op1) {
        /*     double x=op1;*/
        return("ADOL-C error: illegal argument combination for operator <<=\n");
    };
    err_retu operator+= (double op1) {
        /*     double x=op1;*/
        return("ADOL-C error: illegal argument combination for operator +=\n");
    };
    err_retu operator-= (double op1) {
        /*     double x=op1;*/
        return("ADOL-C error: illegal argument combination for operator -=\n");
    };
    inline ADOLC_DLL_EXPORT friend err_retu operator+(const badoublev &op1,double op2) {
        /*     badoublev y=op1;
             double x=op2;*/
        return("ADOL-C error: illegal argument combination for operator +\n");
    };
    inline ADOLC_DLL_EXPORT friend err_retu operator-(const badoublev &op1,double op2) {
        /*     badoublev y=op1;
             double x=op2;*/
        return("ADOL-C error: illegal argument combination for operator -\n");
    };
    inline ADOLC_DLL_EXPORT friend err_retu operator+(double op1,const badoublev &op2) {
        /*     badoublev y=op2;
             double x=op1;*/
        return("ADOL-C error: illegal argument combination for operator +\n");
    };
    inline ADOLC_DLL_EXPORT friend err_retu operator-(double op1,const badoublev &op2) {
        /*     badoublev y=op2;
             double x=op1;*/
        return("ADOL-C error: illegal argument combination for operator -\n");
    };
};


/****************************************************************************/
/*                                                              CLASS ADUBV */
class ADOLC_DLL_EXPORT adubv:public badoublev {
    adubv(int lo,int sz) {
        start_loc=lo;
        size=sz;
    };
    /* removed 1/95
      adubv(doublev&);
    */
    adubv():badoublev(0,0) {
        std::cout << "ADOL-C error: illegal default construction of adub variable\n" ;
        exit(-2);
    };

public:
    /* removed 1/95
      ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, const doublev &op2);
      ADOLC_DLL_EXPORT friend adub operator*(const doublev &op1, const badoublev &op2);
    */
    ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, double* op2);
    ADOLC_DLL_EXPORT friend adub operator*(double* op1, const badoublev &op2);
    ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, const badoublev &op2);
    /* excluded because g++ warnings
    ADOLC_DLL_EXPORT friend adub operator*(const badoublev &op1, const doublev &op2);
    ADOLC_DLL_EXPORT friend adub operator*(const doublev &op1, const badoublev &op2);
    */
    /* removed 1/95
      ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1, const doublev &op2);
      ADOLC_DLL_EXPORT friend adubv operator+(const doublev &op1, const badoublev &op2);
      ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1, const doublev &op2);
      ADOLC_DLL_EXPORT friend adubv operator-(const doublev &op1, const badoublev &op2);
      ADOLC_DLL_EXPORT friend adubv operator/(const doublev &op1, const badouble &n);
      ADOLC_DLL_EXPORT friend adubv operator*(const doublev &op1, const badouble &n);
      ADOLC_DLL_EXPORT friend adubv operator*(const badouble &n, const doublev &op1);
    */
    ADOLC_DLL_EXPORT friend adubv operator/(const badoublev &op1, const badouble &n);
    inline ADOLC_DLL_EXPORT friend adubv operator/(const badoublev &op1, double n);
    ADOLC_DLL_EXPORT friend adubv operator+ (const badoublev &x);
    ADOLC_DLL_EXPORT friend adubv operator- (const badoublev &x);
    ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1,const badoublev &op2);
    ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1,const badoublev &op2);
    ADOLC_DLL_EXPORT friend adubv operator*(const badoublev &op1, double n);
    ADOLC_DLL_EXPORT friend adubv operator*(double n, const badoublev &op1);
    /* excluded because g++ warnings
    ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1, const doublev &op2);
    ADOLC_DLL_EXPORT friend adubv operator+(const doublev &op1, const badoublev &op2);
    */
    ADOLC_DLL_EXPORT friend adubv operator+(const badoublev &op1, double* op2);
    ADOLC_DLL_EXPORT friend adubv operator+(double* op2, const badoublev &op1);
    /* excluded because g++ warnings
    ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1, const doublev &op2);
    ADOLC_DLL_EXPORT friend adubv operator-(const doublev &op1, const badoublev &op2);
    */
    ADOLC_DLL_EXPORT friend adubv operator-(const badoublev &op1, double* op2);
    ADOLC_DLL_EXPORT friend adubv operator-(double* op1, const badoublev &op2);
    /* excluded because g++ warnings
    ADOLC_DLL_EXPORT friend adubv operator/(const doublev &op1, const badouble &n);
    */
    ADOLC_DLL_EXPORT friend adubv operator*(const badoublev &op1, const badouble &n);
    ADOLC_DLL_EXPORT friend adubv operator*(const badouble &n, const badoublev &op1);
#ifdef overwrite
    ~adubv();
#endif
};


/****************************************************************************/
/*                                                           CLASS ADOUBLEV */
class ADOLC_DLL_EXPORT adoublev:public badoublev {
    ADOLC_DLL_EXPORT friend class adoublem;
    adoublev() {};
public:
    adoublev(const adubv& a);
    adoublev(const adoublev&);
    adoublev(int sz);
//  adoublev(int n, double *values);
    /* removed 1/95
      adoublev(doublev&);
    */

#ifdef overwrite
    ~adoublev();
#endif
    /* removed 1/95
      adoublev& operator= (const doublev &y);
    */
    adoublev& operator= (const badoublev&);
    adoublev& operator= (const adoublev&);
    adoublev& operator= (const adubv&);
    adoublev& operator= (double y);
    adoublev& operator= (double* y);
    /* removed 1/95
      adoublev& operator >>= (doublev& );
      adoublev& operator <<= (doublev& );
    */
    adoublev& operator >>= (double* );
    adoublev& operator <<= (double* );
};

/*--------------------------------------------------------------------------*/
inline adubv operator / (const badoublev& x, double y) {
    return (1.0/y)*x;
}


/****************************************************************************/
/*                                                           CLASS ADOUBLEM */
class ADOLC_DLL_EXPORT adoublem {
    int n, m;          /* Size of the matrix */
    adoublev *index;     /* So each row is an adoublev */
public:
    adoublem(int n, int m);
    adoublem(const adoublem& );
    ~adoublem();
    adoublev& operator[](int i);  /* Can access component like an array */
    asubv operator[](const along&);
};


/****************************************************************************/
/*                                                              CLASS ASUBV */
class ADOLC_DLL_EXPORT asubv:public badoublev {
    locint base,offset,begin;
public:
    asubv(adoublev* start, locint index);
#ifdef overwrite
    ~asubv();
#endif
    /* removed 1/95
      asubv& operator <<= (doublev&);
      asubv& operator = (doublev);
    */
    asubv& operator <<= (double*);
    asubv& operator = (double*);
    asubv& operator = (const adubv&);
    asubv& operator = (const badoublev&);
    /* added Sep/01/96 */
    asubv& operator = (const asubv&);
    asubv& operator += (const badoublev&);
    asubv& operator -= (const badoublev&);
    /* there are currently no +=, -= operators for double*
       right hand sides. They woudl require a special treatment 
       similar to the assignment operators caused by the buffered
       writing of the constant right hand side to the tape. */
    asubv& operator *= (double x);
    asubv& operator *= (const badouble&);
    asubv& operator /= (double x);
    asubv& operator /= (const badouble&);
};

/****************************************************************************/
/*                                                                THAT'S ALL*/

#endif /* __cplusplus */
#endif /* ADOLC_TAPELSS */
#endif /* ADOLC_AVECTOR_H */
