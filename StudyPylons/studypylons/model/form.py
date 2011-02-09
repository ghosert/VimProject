import formencode

# jiawzhang: Add annotation on person.py controller, and something like '${h.form blabal' on person.makao, it will present error automatically if validation failed.
class EmailForm(formencode.Schema):
    # We have only email inputbox and submit button on person.mako page, Allow extra true means allow extra fields passed in.
    allow_extra_fields = True
    # But ignore the extra fields.
    filter_extra_fields = True
    # validator for email, not_empty=True means field required.
    email_validation_field = formencode.validators.Email(not_empty=True)

