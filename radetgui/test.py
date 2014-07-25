# -*- coding: utf-8 -*-
"""
Created on Thu Jul 24 11:57:50 2014

@author: Roma
"""

import urllib.request as urq
from html.parser import HTMLParser
from urllib.parse import urljoin

class DataParse(HTMLParser):
    trTag = False
    tdTag = False
    aTag = False
    tdcnt = 0
    isotFlag = False
    atomFlag = False
    atomData = ""
    isotData = ""
    elements = {}
    def handle_starttag(self, tag, attrs):
        if (self.trTag and tag == 'a'):
            self.get_addrData(attrs)
            self.aTag = True
        if (self.trTag and tag == 'td'):
            self.tdcnt += 1
            self.tdTag = True
        if (self.trTag and self.tdTag and tag == 'br'):
            self.atomFlag = True
        if (tag == 'tr'):
            self.trTag = True
            self.tdcnt = 0
            self.atomFlag = False
            self.isotFlag = False
            
    def handle_endtag(self, tag):
        if (tag == 'tr'):
            self.trTag = False
            self.tdcnt = 0
        if (tag == 'a'):
            self.aTag = False
        if (tag == 'td'):
            self.tdFlag = False
    
    def handle_data(self, data):
        if (self.trTag and self.aTag and data == "raw eval"):
            self.get_datarow()
        if (self.trTag and self.tdcnt == 1 and self.tdTag and not self.atomFlag):
            self.atomData = data
            self.atomFlag = True
        if (self.trTag and self.tdcnt == 2 and self.tdTag and not self.isotFlag):
            self.isotData = data
            self.isotFlag = True
                
    def get_datarow(self):
        if (self.atomData not in self.elements):
            self.elements[self.atomData] = []
        self.elements[self.atomData].append((self.isotData, self.addrData))
        
    def get_addrData(self, attrs):
        for t in attrs:
            if (t[0] == 'href'):
                self.addrData = t[1]
                return
        self.addrData = ""
        
  #  def get_data(self):
  #      self.tdData
        
parser = DataParse()
A = urq.urlopen("http://t2.lanl.gov/nis/data/endf/endfvii.1-n.html")
parser.feed(repr(A.read()))
parser.url = "http://t2.lanl.gov/nis/data/endf/endfvii.1-n.html"
A = urq.urlopen(urljoin(parser.url, parser.elements["1-H"][0][1]))
LA=A.read().decode().split('\n')