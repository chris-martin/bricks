from django.views.generic import *

from .models import *


class HomeView(TemplateView):
    template_name = 'home.html'


class SchoolView(ListView):
    model = Course
    template_name = 'school.html'
