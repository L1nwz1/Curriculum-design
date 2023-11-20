library(glmnet)
library(ncvreg)

set.seed(123)

# 模拟参数
p <- 100
n <- 100
rho <- c(0.1, 0.5, 0.9)
beta <- c(rep(1, 5), rep(0.5, 5), rep(0, p-10))
num_simulations <- 100
# 初始化结果向量
fnr_lasso <- fnr_mcp <- fnr_scad <- rep(0, length(rho))
fdr_lasso <- fdr_mcp <- fdr_scad <- rep(0, length(rho))

for (sim in 1:num_simulations) {
  for (r in 1:length(rho)) {
    # 生成相关矩阵
    Sigma <- matrix(rho[r], nrow = p, ncol = p)
    diag(Sigma) <- 1
    
    # 生成数据
    X <- MASS::mvrnorm(n, rep(0, p), Sigma)
    y <- X %*% beta + rnorm(n)
    
    # LASSO回归
    fit_lasso <- cv.glmnet(X, y, alpha = 1)
    coef_lasso <- coef(fit_lasso, s = "lambda.min")
    selected_vars_lasso <- which(coef_lasso[-1] != 0)
    
    # MCP回归
    fit_mcp <- ncvreg(X, y, penalty = "MCP")
    coef_mcp <- coef(fit_mcp)
    selected_vars_mcp <- which(coef_mcp[-1] != 0)
    
    # SCAD回归
    fit_scad <- ncvreg(X, y, penalty = "SCAD")
    coef_scad <- coef(fit_scad)
    selected_vars_scad <- which(coef_scad[-1] != 0)
    
    # 计算FNR和FDR
    true_vars <- which(beta != 0)
    fnr_lasso[r] <- fnr_lasso[r] + length(setdiff(true_vars, selected_vars_lasso)) / length(true_vars)
    fdr_lasso[r] <- fdr_lasso[r] + length(setdiff(selected_vars_lasso, true_vars)) / length(selected_vars_lasso)
    
    fnr_mcp[r] <- fnr_mcp[r] + length(setdiff(true_vars, selected_vars_mcp)) / length(true_vars)
    fdr_mcp[r] <- fdr_mcp[r] + length(setdiff(selected_vars_mcp, true_vars)) / length(selected_vars_mcp)
    
    fnr_scad[r] <- fnr_scad[r] + length(setdiff(true_vars, selected_vars_scad)) / length(true_vars)
    fdr_scad[r] <- fdr_scad[r] + length(setdiff(selected_vars_scad, true_vars)) / length(selected_vars_scad)
  }
}

# 计算平均值
fnr_lasso <- fnr_lasso / num_simulations
fdr_lasso <- fdr_lasso / num_simulations
fnr_mcp <- fnr_mcp / num_simulations
fdr_mcp <- fdr_mcp / num_simulations
fnr_scad <- fnr_scad / num_simulations
fdr_scad <- fdr_scad / num_simulations

# 输出结果
result <- data.frame(rho, fnr_lasso, fdr_lasso, fnr_mcp, fdr_mcp, fnr_scad, fdr_scad)
print(result)

