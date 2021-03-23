

// # model formula
// # adj: r0*p_n1 + (1-r0)*g
// # n1: r1*p_n1 + (1-r1)*g
// # n2: r2*(1-p_n1) + (1-r2)*g
// # amb: h * (r1*p_n1 + (1-r1)*g) +  (1-h) * (r1*p_n1 + (1-r1)*g)

// model r1r2hg
real logodds_yes_model(real condition, real probe_n1, real r1, real r2, real g, real h)
{
  real prob_yes;
  
  if (condition == 1) { // N1
      prob_yes = r1*probe_n1 + (1-r1)*g;

  } else if (condition == 2) { // N2
      prob_yes = r2*(1-probe_n1) + (1-r2)*g;

  } else if (condition == 3) { // ambiguous
      prob_yes = h * (r1*probe_n1 + (1-r1)*g) +  (1-h) * (r2*(1-probe_n1) + (1-r2)*g);
  }
  
  return logit(prob_yes);
}


real logodds_yes_model_ahg(real condition, real probe_n1, real a, real g, real h)
{
  real prob_yes;
  
  if (condition == 1) { // N1
      prob_yes = a*probe_n1 + (1-a)*g;

  } else if (condition == 2) { // N2
      prob_yes = a*(1-probe_n1) + (1-a)*g;

  } else if (condition == 3) { // ambiguous
      prob_yes = a * ( h * probe_n1 + (1 - h) * (1 - probe_n1 ) ) + (1 - a) * g;
  }
  
  return logit(prob_yes);
}



// real logodds_yes_model_r1r2hg1g2f(real condition, real probe_n1, real r1, real r2, real h, real g1, real g2, real f)
// {
//   real prob_yes;
//   
//   real prob_guess_yes = f * g1 + (1-f) * ( probe_n1 * g2 + (1-probe_n1) * (1-g2) );
// 
//   if (condition == 1) { // N1
//       prob_yes = r1*probe_n1 + (1-r1)*prob_guess_yes;
// 
//   } else if (condition == 2) { // N2
//       prob_yes = r2*(1-probe_n1) + (1-r2)*prob_guess_yes;
// 
//   } else if (condition == 3) { // ambiguous
//       prob_yes = h * (r1*probe_n1 + (1-r1)*prob_guess_yes) +  (1-h) * (r2*(1-probe_n1) + (1-r2)*prob_guess_yes);
//   }
//   
//   return logit(prob_yes);
// }
// 
// real logodds_yes_model_ag1g2fh(real condition, real probe_n1, real a, real g1, real g2, real f, real h)
// {
//   real prob_yes;
//   
//   real prob_guess_yes = f * g1 + (1-f) * ( probe_n1 * g2 + (1-probe_n1) * (1-g2) );
//   
//   if (condition == 1) { // N1
//       prob_yes = a * probe_n1 + (1-a) * prob_guess_yes;
// 
//   } else if (condition == 2) { // N2
//       prob_yes = a * (1-probe_n1) + (1-a) * prob_guess_yes;
// 
//   } else if (condition == 3) { // ambiguous
//       prob_yes = a * ( h * probe_n1 + (1 - h) * (1 - probe_n1 ) ) + (1 - a) * prob_guess_yes;
//   }
//   
//   return logit(prob_yes);
// }

