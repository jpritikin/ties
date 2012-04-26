sim.categorical <- function(person, item, discr, levels) {
  pick <- rnorm(length(person), person - item, sd=discr)
  return(apply(as.matrix(pick), c(1), function (p) { sum(p > levels) }))
}
sim.familiar <- function(person, item, discr) {
  sim.categorical(person, item, discr, qnorm(c(.2,.8))) }
sim.context <- function(person, item, discr) {
  sim.categorical(person, item, discr, qnorm(c(.05,.3,.7,.95))) }
cms <- data.frame(
  notSense=sim.familiar(cms.latent$familiar, -2, .8),
  possAny=sim.familiar(cms.latent$familiar, -1.5, 1.6),
  possMyself=sim.familiar(cms.latent$familiar, -1, 1.1),
  accident=sim.familiar(cms.latent$familiar, 0, 1.3),
  intention=sim.familiar(cms.latent$familiar, 1, .6),
  certain=sim.familiar(cms.latent$familiar, 1.5, .9),
  
  cxReligion=sim.context(cms.latent$familiar, 0, 1.5),
  cxSolving=sim.context(cms.latent$familiar +
    unclass(cms.latent$edu)/3, 0, .8),
  cxSpirit=sim.context(cms.latent$familiar, -.5, 1.3),
  cxRelax=sim.context(cms.latent$familiar, 0, 1.5),
  cxDaydream=sim.context(cms.latent$familiar, 1, .9),
  cxMeditate=sim.context(cms.latent$familiar, -1, 1.5),
  cxExercise=sim.context(cms.latent$familiar, 0, 1.7))
