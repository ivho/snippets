from django.utils.translation import ugettext as _
from django.utils.encoding import force_unicode
from django.http import HttpResponse, HttpResponseRedirect
from django.core.urlresolvers import reverse
from django.template import Context, loader


class Kalle(object):
    def __init__(self, a, b):
        self.title = a
        self.body = b
    
# Create your views here.
def main(response):
    t = loader.get_template('sample.html')
    c = Context({
                 'title':'MY TITLE',
                 'body':'HEJ',
                 'header':'MY HEADER',
                 'weeks':[29,30,31],
                 'blog_entries':{Kalle("HEJ","HA"), Kalle("SVENNE","APA")}
                 })
    return HttpResponse(t.render(c))

def weeks():
    return ["aaa", "bb", "c"]

from django.shortcuts import render_to_response
from django.template import RequestContext

def my_view(request):
    return render_to_response('base.html',
                              RequestContext(request,
                                             {'data':data,}))


def kalle():
    return "KALLE"
