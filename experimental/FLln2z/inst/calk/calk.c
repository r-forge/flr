#include <stdio.h>
#include <math.h>
#define NI 100
#define NJ 30
#define NK 10
#define NH 20

/* Starts main function */ 
  void
main () 
{
     FILE * Infile, *Outfile;
     int i, j, k, h, I, J, K, H;
     int iteration = 0;
     float dn[NI][NJ][NK];
     float control = 1;
     float sumdif, pinit, x, parc;
     float p1[NJ][NK];		/* array of age proportions in years with age data */
     float p1old[NJ][NK];
     float p2[NJ][NH];
     float p2old[NJ][NH];
     float data[NI][NJ * NK + NK + NH];	/* initial data */
     float d[NI][NJ][NK];		/* array of ALKs */
     float v[NI][NK];		/* length distributions array */
     float m[NI][NJ][NK];		/* array of lengths*ages*years */
     float m2[NI][NJ][NH];
     float inv[NI][NJ];
     float v2[NI][NH];
     float vecage1[NJ][NK];
     float vtot[NK];
     float mtot[NI][NJ];
     float m2tot[NI][NJ];
     float vecagetot[NJ];
     float v2tot[NH];
     float vecage2[NJ][NH];
     float vecagetot2[NJ];
     float prob1[NI][NK];
     float prob2[NI][NH];
     float fast[NI][NK];
     char FILE_NAME[20];
        printf ("\n\nCOMBINED AGE-LENGTH KEYS \n");
     printf ("(1999) Alberto G. Murta, IPIMAR - Lisbon \n ");
        printf ("\nName of data file?(max. 20 chars) ");
     scanf ("%s", &FILE_NAME);
        printf ("Number of length classes? ");
     scanf ("%d", &I);
        printf ("Number of age classes? ");
     scanf ("%d", &J);
     pinit = 1.0 * J;
        printf ("Number of data sets WITH age data? ");
     scanf ("%d", &K);
        printf ("Number of data sets WITHOUT age data? ");
     scanf ("%d", &H);
        printf ("\nReading data file...");
     
/* Reads file into matrix data */ 
    Infile = fopen (FILE_NAME, "r");
     for (i = 0; i < I; i++)
    
    {
         for (j = 0; j < J * K + K + H; j++)
	
	{
	     fscanf (Infile, "%f", &x);
	     data[i][j] = x;
	   }
       }
     fclose (Infile);
     printf ("done. \n");
        printf ("\nCalculating initial values...");
     
/* Creates array dn */ 
    for (k = 0; k < K; k++)
    
    {
         for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         dn[i][j][k] = data[i][j + J * k];
	       }
	   }
       }
     
/* Creates array fast */ 
    for (k = 0; k < K; k++)
    
    {
         for (i = 0; i < I; i++)
	
	{
	     parc = 0;
	     for (j = 0; j < J; j++)
	    
	    {
	         parc = parc + dn[i][j][k];
	       }
	     fast[i][k] = parc;
	   }
       }
     
/* Creates array d */ 
    for (k = 0; k < K; k++)
    
    {
         for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         d[i][j][k] = dn[i][j][k] / fast[i][k];
	       }
	   }
       }
     
/* Creates array v */ 
    for (i = 0; i < I; i++)
    
    {
         for (k = 0; k < K; k++)
	
	{
	     v[i][k] = data[i][k + K * J];
	   }
       }
     
/* Creates array m */ 
    for (k = 0; k < K; k++)
    
    {
         for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         m[i][j][k] = d[i][j][k] * v[i][k];
	       }
	   }
       }
     
/* Creates array p1 */ 
    for (j = 0; j < J; j++)
    
    {
         for (k = 0; k < K; k++)
	
	{
	     p1[j][k] = 1 / pinit;
	   }
       }
     
/* Creates array p2 */ 
    for (j = 0; j < J; j++)
    
    {
         for (h = 0; h < H; h++)
	
	{
	     p2[j][h] = 1 / pinit;
	   }
       }
     
/* Creates array mtot */ 
    for (i = 0; i < I; i++)
    
    {
         for (j = 0; j < J; j++)
	
	{
	     parc = 0;
	     for (k = 0; k < K; k++)
	    
	    {
	         parc = parc + m[i][j][k];
	       }
	     mtot[i][j] = parc;
	   }
       }
     
/* Creates array vtot  */ 
    for (k = 0; k < K; k++)
    
    {
         parc = 0;
         for (i = 0; i < I; i++)
	
	{
	     parc = parc + v[i][k];
	   }
         vtot[k] = parc;
       }
     
/* Creates array vecagetot */ 
    for (j = 0; j < J; j++)
    
    {
         parc = 0;
         for (k = 0; k < K; k++)
	
	{
	     for (i = 0; i < I; i++)
	    
	    {
	         parc = parc + m[i][j][k];
	       }
	   }
         vecagetot[j] = parc;
       }
     
/* Creates array inv */ 
    for (i = 0; i < I; i++)
    
    {
         for (j = 0; j < J; j++)
	
	{
	     inv[i][j] = mtot[i][j] / vecagetot[j];
	   }
       }
     
/* Creates array v2 */ 
    for (i = 0; i < I; i++)
    
    {
         for (h = 0; h < H; h++)
	
	{
	     v2[i][h] = data[i][h + K * J + K];
	   }
       }
     
/* Creates array v2tot */ 
    for (h = 0; h < H; h++)
    
    {
         parc = 0;
         for (i = 0; i < I; i++)
	
	{
	     parc = parc + v2[i][h];
	   }
         v2tot[h] = parc;
       }
     
/* Creates array m2 */ 
    for (i = 0; i < I; i++)
    
    {
         for (j = 0; j < J; j++)
	
	{
	     for (h = 0; h < H; h++)
	    
	    {
	         m2[i][j][h] = v2tot[h] * inv[i][j] * p2[j][h];
	       }
	   }
       }
        printf ("done. \n");
     
/* STARTS ITERATIONS */ 
       printf ("\nStarting iterative process (EM algorithm)... \n");
     while (control >= 0.001)
    
    {
         
/* Starts iterative cycle */ 
	iteration = iteration + 1;
         
/* Creates array vecage1 */ 
	for (k = 0; k < K; k++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         parc = 0;
	         for (i = 0; i < I; i++)
		
		{
		     parc = parc + m[i][j][k];
		   }
	         vecage1[j][k] = parc;
	       }
	   }
         
/* Creates array vecage2 */ 
	for (h = 0; h < H; h++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         parc = 0;
	         for (i = 0; i < I; i++)
		
		{
		     parc = parc + m2[i][j][h];
		   }
	         vecage2[j][h] = parc;
	       }
	   }
         
/* Creates array p1old */ 
	for (j = 0; j < J; j++)
	
	{
	     for (k = 0; k < K; k++)
	    
	    {
	         p1old[j][k] = p1[j][k];
	       }
	   }
         
/* Creates array p2old */ 
	for (j = 0; j < J; j++)
	
	{
	     for (h = 0; h < H; h++)
	    
	    {
	         p2old[j][h] = p2[j][h];
	       }
	   }
         
/* Recalculates array p1 */ 
	for (k = 0; k < K; k++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         p1[j][k] = vecage1[j][k] / vtot[k];
	       }
	   }
         
/* Recalculates array p2 */ 
	for (h = 0; h < H; h++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         p2[j][h] = vecage2[j][h] / v2tot[h];
	       }
	   }
         
/* Recalculates control */ 
	sumdif = 0;
         for (j = 0; j < J; j++)
	
	{
	     for (k = 0; k < K; k++)
	    
	    {
	         sumdif = sumdif + fabs (p1old[j][k] - p1[j][k]);
	       }
	   }
            for (j = 0; j < J; j++)
	
	{
	     for (h = 0; h < H; h++)
	    
	    {
	         sumdif = sumdif + fabs (p2old[j][h] - p2[j][h]);
	       }
	   }
         control = sumdif;
         
/* Recalculates array mtot */ 
	for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         parc = 0;
	         for (k = 0; k < K; k++)
		
		{
		     parc = parc + m[i][j][k];
		   }
	         mtot[i][j] = parc;
	       }
	   }
         
/* Calculates array m2tot */ 
	for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         parc = 0;
	         for (h = 0; h < H; h++)
		
		{
		     parc = parc + m2[i][j][h];
		   }
	         m2tot[i][j] = parc;
	       }
	   }
         
/* Recalculates array vecagetot */ 
	for (j = 0; j < J; j++)
	
	{
	     parc = 0;
	     for (k = 0; k < K; k++)
	    
	    {
	         for (i = 0; i < I; i++)
		
		{
		     parc = parc + m[i][j][k];
		   }
	       }
	     vecagetot[j] = parc;
	   }
         
/* Calculates array vecagetot2 */ 
	for (j = 0; j < J; j++)
	
	{
	     parc = 0;
	     for (h = 0; h < H; h++)
	    
	    {
	         for (i = 0; i < I; i++)
		
		{
		     parc = parc + m2[i][j][h];
		   }
	       }
	     vecagetot2[j] = parc;
	   }
         
/* Recalculates array inv */ 
	for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         inv[i][j] =
		(mtot[i][j] + m2tot[i][j]) / (vecagetot[j] + vecagetot2[j]);
	       }
	   }
         
/* Calculates array prob1 */ 
	for (k = 0; k < K; k++)
	
	{
	     for (i = 0; i < I; i++)
	    
	    {
	         parc = 0;
	         for (j = 0; j < J; j++)
		
		{
		     parc = parc + inv[i][j] * p1[j][k];
		   }
	         prob1[i][k] = parc;
	       }
	   }
         
/* Calculates array prob2 */ 
	for (h = 0; h < H; h++)
	
	{
	     for (i = 0; i < I; i++)
	    
	    {
	         parc = 0;
	         for (j = 0; j < J; j++)
		
		{
		     parc = parc + inv[i][j] * p2[j][h];
		   }
	         prob2[i][h] = parc;
	       }
	   }
         
/* Recalculates array m */ 
	for (k = 0; k < K; k++)
	
	{
	     for (i = 0; i < I; i++)
	    
	    {
	         for (j = 0; j < J; j++)
		
		{
		     m[i][j][k] =
		    d[i][j][k] * fast[i][k] + (v[i][k] -
					       fast[i][k]) * inv[i][j] *
		    p1[j][k] / prob1[i][k];
		   }
	       }
	   }
         
/* Recalculates array m2 */ 
	for (h = 0; h < H; h++)
	
	{
	     for (i = 0; i < I; i++)
	    
	    {
	         for (j = 0; j < J; j++)
		
		{
		     m2[i][j][h] =
		    v2[i][h] * inv[i][j] * p2[j][h] / prob2[i][h];    }
	       }
	   }
            if (iteration == 3000)
	
	{
	     printf
	    ("Did not converge after 3000 iterations.\nPlease check your data.\n ");
	     goto stop;
	   }
          }				/* Ends iterative cycle */
     printf ("Converged after %d iterations. \n", iteration + 1);
     
/* Writes output file */ 
    printf ("\nWriting output file...");
     Outfile = fopen ("calk.out", "w");
        fprintf (Outfile, "COMBINED AGE-LENGTH KEYS \n");
     fprintf (Outfile, "(1999) Alberto G. Murta, IPIMAR - Lisbon \n \n ");
        fprintf (Outfile, "Inverse Age-Length Key: \n");
     for (i = 0; i < I; i++)
    
    {
         for (j = 0; j < J; j++)
	
	{
	     fprintf (Outfile, "%4.4f ", inv[i][j]);
	   }
         fprintf (Outfile, "\n");
       }
     fprintf (Outfile, "\n");
        fprintf (Outfile, "Number of individuals by length and age classes: \n");
     for (k = 0; k < K; k++)
    
    {
         for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         fprintf (Outfile, "%4.0f ", m[i][j][k]);
	       }
	     fprintf (Outfile, "\n");
	   }
         fprintf (Outfile, "\n");
       }
     for (h = 0; h < H; h++)
    
    {
         for (i = 0; i < I; i++)
	
	{
	     for (j = 0; j < J; j++)
	    
	    {
	         fprintf (Outfile, "%4.0f ", m2[i][j][h]);
	       }
	     fprintf (Outfile, "\n");
	   }
         fprintf (Outfile, "\n");
       }
        fprintf (Outfile, "Proportion of each age class in each data set: \n");
     for (k = 0; k < K; k++)
    
    {
         for (j = 0; j < J; j++)
	
	{
	     fprintf (Outfile, "%4.4f ", p1[j][k]);
	   }
         fprintf (Outfile, "\n");
       }
     for (h = 0; h < H; h++)
    
    {
         for (j = 0; j < J; j++)
	
	{
	     fprintf (Outfile, "%4.4f ", p2[j][h]);
	   }
         fprintf (Outfile, "\n");
       }
        fclose (Outfile);
     printf ("done. \n");
     printf ("Results are in output file CALK.OUT. \n");
   stop:   printf ("\nTerminated. \n\n");
   }

   
