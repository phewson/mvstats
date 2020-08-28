Multivariate Statistics with R
=====================================


This was a project I was working on a decade ago, when I was teaching the subject with great enthusiasm.   There are a set of Acrotex worksheets as well (to be found and added).  The whole idea was to provide an introductory level exposition of the "Multivariate Canon", with manageable datasets.  The kind of thing where you could do all the computations by hand (not entirely by hand, but with a calculator to do some of the matrix operations).  It seemed to be important to some learners that they could see how this worked.  But having done this, to provide more interesting and modern illustrations.   I accumulated a lot of (in my opinion) interesting datasets from various journal publications to show off modern applications of multivariate methods to modern problems.

One of the things I was struck by is that:

- Dimension reduction
- Supervised and unsupervised classification

are handled very differently in Computer Science (Machine Learning) than they are in Statistics, yet we are trying to solve the same problem. There are some real academic silos here. There are some nice ideas that seem to be more from the Computer Science side (leave one out cross-validation) and some nice ideas from Statistics (Inference).   Above all the fine details, these seem like important threshold concepts to get well set out.

Anyway, the current material is in **quite** a mess. My first job is to remove the mess. The plan was to have a set of chapters run from .Rnw files that can be Sweaved.   I've learnt a lot since about CI/CD and the implications of managing this correctly (i.e., not panicking when I realise a verion change has broken a lot of stuff).  I don't know if I will include the Acrotex exercises at all; I never did figure out how to collect the results back, and the *exams* package in R had a lovely Moodle integration.   I will be adding the modern examples in.   And if it's still there, I will remove any use of the Iris data. I forget what the recommended replacement is, but I will check and make the switch.

I have recovered health wise in the last few months, and will be putting some energy into this. But I'm very happy to talk to anyone who would like to join in.
