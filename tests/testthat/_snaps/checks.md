# Messages are created for missing parameters

    Code
      perform_checks(sample_rules, pos = 1, neg = NULL, z = 0)
    Message <message>
      `negative` not run because parameter `neg` not specified
      `neg_or_zero` not run because parameter `neg` not specified
    Output
         positive    negative        zero neg_or_zero 
              "P"          NA         "P"          NA 

---

    Code
      perform_checks(sample_rules, pos = 1, neg = NULL, z = NULL)
    Message <message>
      `negative` not run because parameter `neg` not specified
      `zero` not run because parameter `z` not specified
      `neg_or_zero` not run because parameters `neg`, `z` not specified
    Output
         positive    negative        zero neg_or_zero 
              "P"          NA          NA          NA 

