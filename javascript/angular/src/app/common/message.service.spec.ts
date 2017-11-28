import { TestBed, inject } from '@angular/core/testing'
import { HttpClientModule } from '@angular/common/http'

import { MessageService } from './message.service'

describe('MessageService', () => {
  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ HttpClientModule ],
      providers: [ MessageService ]
    })
  })

  it('should be created', inject([ MessageService ], (service: MessageService) => {
    expect(service).toBeTruthy()
  }))
})
