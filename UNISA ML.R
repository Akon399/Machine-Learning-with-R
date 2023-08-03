# Linear Model notes.

unisa = read.csv("C:/Users/User/Desktop/UNISA.csv", sep = ";")
head(unisa)


# Model Development.

model = lm(y ~ x, data = unisa)
print(summary(model))

# 1. Slope = m = 0.87265 = B_1 = x & Estimate.
# 2. Intercept = c = 10.72 = B_0 = (Intercept) & Estimate.
# y = mx + c.
# Then our Linear model is : y = 0.87x + 10.72.