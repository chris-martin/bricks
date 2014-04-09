from django.db.models import *


class Course(Model):
    year = IntegerField()
    semester = CharField(max_length=20)
    subject = CharField(max_length=10)
    number = CharField(max_length=10)
    hours = IntegerField()
    grade = CharField(max_length=5)
    title = CharField(max_length=100)

    @property
    def semester_and_year(self):
        return '%s %s' % (self.semester, self.year)

    @property
    def chronological_index(self):
        return self.year + {
            'Spring': 0.1,
            'Summer': 0.2,
            'Fall'  : 0.3
        }[self.semester]
