#'중심극한정리

#'중심극한정리를 카이제곱을 이용하여 직접 확인할수있다

#' CLT.plot(rchisq, n = 1, df = 1)
#' CLT.plot(rchisq, n = 30, df = 1)일때를 비교

CLT.plot <- function(r.dist, n, ...) {

  means <- double()

  for(i in 1:1000) means[i] = mean(r.dist(n,...))
  std.means <- scale(means)
  par(mfrow = c(1, 2))
  hist(std.means, prob = T, col = "light grey",
       border = "grey", main = NULL, ylim = c(0, 0.5))
  lines(density(std.means))
  box()
  curve(dnorm(x, 0, 1), -3, 3, col = 'blue', add = T)
  qqnorm(std.means, main="", cex = 0.8)
  abline(0, 1, lty = 2, col = "red")
  par(mfrow = c(1, 1))
}
