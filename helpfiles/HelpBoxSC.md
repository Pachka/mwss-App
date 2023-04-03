## Surveillance and control measures considered
***
### Isolation/contact restrictions

When imposing isolation/contact restrictions to detected patients, the model considers that the risk of transmission during contacts with detected patients is null.

### Random tests at regular intervals

For patients, the model considers that, every week, 75% of patients with no vaccinate or infection history are randomly tested every week, 50% of patients with old vaccinate or infection history and 10% of patients with old vaccinate or infection history.

For professionals, the model considers that, every two weeks, 75% of professionals with no vaccinate or infection history are randomly tested every week, 50% of professionals with old vaccinate or infection history and 20% of professionals with old vaccinate or infection history.

Note that the model considers that a tested individual will not be tested again before a minimal duration fixed as the duration of the disease (eg 10 days for covid).


### Test at patient admission

The model considers that, before final admission, new patients will undergo a clinical examination and can be tested. The model considers that symptomatic patients are always tested, and among asymptomatic or susceptible patients: 75% of patients with no vaccinate or infection history, 50% of patients with old vaccinate or infection history and 25% of patients with old vaccinate or infection history will undergo a test. The model considers 2 contacts of 10min with the professional in charge of admission and no contact with other patients.
