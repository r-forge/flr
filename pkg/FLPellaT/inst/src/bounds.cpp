dvariable _boundp(dvariable x, double fmin, double fmax, dvariable& fpen)
   {
   dvariable t, y;

   t = fmin + (fmax-fmin)*(sin(x*1.570795)+1.0)/2.0;

   if (x < 0.00001)
      fpen += (x-0.00001)*(x-0.00001);

   if (x > 0.9999)
      fpen += (x-0.9999)*(x-0.9999);

   if (x < -1.0)
      fpen += 1000.0*(x+1.0)*(x+1.0);

   if (x > 1.0)
      fpen += 1000.0*(x-1.0)*(x-1.0);

   return t;
   }

double AutoDiff::Boundpin(FLTypeADControl *pAD, int iParam)
   {
   double Val, L, U;

   if (ActivePhaseSeries(*pAD))
      {
      for (int j = pAD->Min; j <= pAD->Max; j++)
         if (pAD->_Param[j] == iParam)
            {
            Val = pAD->_BestGuess[j];
            L   = pAD->_Lower[    j];
            U   = pAD->_Upper[    j];

            break;
            }
      }
   else
      {
      Val = pAD->BestGuess;
      L   = pAD->Lower;
      U   = pAD->Upper;
      }

   if (Val < L || Val > U)
      Val = (L + U)/2.0;

   if (Val == 0.0)
      return boundpin(Val+1.0, L+1.0, U+1.0);

   if (fabs(Val) < 0.1)
      if (Val > 0.0)
         return boundpin(1.0, L/Val, U/Val);
      else
         return boundpin(1.0, U/Val, L/Val);

   return boundpin(Val, L, U);
   }