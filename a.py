# Import system modules
import os
import win32com.client
import win32com.client.makepy

# Generate type library so that we can access constants
win32com.client.makepy.GenerateFromTypeLibSpec('Acrobat')

def convertHTML2PDF(htmlPath, pdfPath):
    'Convert an HTML document to PDF format'
    # Connect to Adobe Acrobat
    avDoc = win32com.client.Dispatch('AcroExch.AVDoc')
    # Open HTML document
    avDoc.Open(os.path.abspath(htmlPath), 'html2pdf')
    # Save in PDF format
    pdDoc = avDoc.GetPDDoc()
    pdDoc.Save(win32com.client.constants.PDSaveFull, os.path.abspath(pdfPath))
    pdDoc.Close()
    # Close HTML document without prompting to save
    avDoc.Close(True)
