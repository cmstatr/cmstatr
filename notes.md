
## From Figure 8.3.1.1(a)

- Failure modes consistent (not part of cmstatr)
- More than 2 conditions (otherwise go to 8.3.1.1(b))
- At least 3 batches, at least 15 specimens at each condition (otherwise go to 8.3.1.1(b))
- Failure modes consistent among environments (not part of cmstatr)
- Test for outliers within each batch and condition (Section 8.3.3.1)
- Test for between-batch variabiliy within each condition with alpha=0.025 (Section 8.3.2.2)
- Pool batches, group by condition only
- Test for outliers within each condition (Section 8.3.3.1)
- Normalize data in each condition to the mean of that condition
- Check normality using Anderson-Darling test with alpha=0.05 (Section 8.3.6.5.1.2)
- Test for equality of variances amoung condition groups with alpha=0.05 (Section 8.3.4.1)
  - Yes: Calcualte basis vlues for pooled data per Section 8.3.5.5.2
  - No: Continue
- Test for equality of normalized varianaces among condtion groups
- Calculate normalized pooled sample mean and SD
- Calculate normalized basis balies for pooled data
- Multiply each condition mean by the normalized basis value to obtain the basis vale for each condition

## From Figure 8.3.1.1(b) (Single-Point)

- Failure modes consistent (not part of cmstatr)
- Test for outliers within each batch (Section 8.3.3.1)
- Test for between-batch variability with alpha=0.025 (Section 8.3.2.2)
- ....


## Notes
- How to deal with batches and environments?
  - Should the user pass variables with the batches and environments?
